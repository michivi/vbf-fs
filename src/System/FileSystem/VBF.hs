{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : System.FileSystem.VBF
-- Description : VBF archive exploration
--
-- This module exposes the main library functions for VBF archive exploration.
--
-- A VBF file is a file archive used by various video games, such as Final
-- Fantasy X HD remaster, to store their various assets (graphics, sounds,
-- videos...).
--
-- This module has several functions that can be used to probe such archives
-- and extract the files they contain.
--
-- Errors are represented as exceptions. Even though a pure version would have
-- been preferrable, this is primarily a quick fun toy project, and some
-- sacrifices were made.
-----------------------------------------------------------------------------
module System.FileSystem.VBF
  ( module System.FileSystem.VBF.Archive
  , module System.FileSystem.VBF.Data
  , module System.FileSystem.VBF.Tree
  , EntryRange(..)
  , ReadingMode(..)
  , vbfContent
  , vbfReadEntryContentLazily
  , vbfWithfExtractedEntry
  )
where

import           System.FileSystem.VBF.Archive
import           System.FileSystem.VBF.Data
import           System.FileSystem.VBF.Tree

import qualified Data.ByteString.Lazy.Char8    as BSL
import qualified Data.Vector                   as Vector
import           Data.Word
import           System.IO               hiding ( hGetContents )

-- | Range of bytes to extract.
data EntryRange
  = EntireFile
  -- ^ The entire file should be extracted.
  | PartialFile Word64 Word64
  -- ^ Only the bytes from the specified offset and size in bytes should be
  -- extracted.
  deriving (Eq, Show)

-- | VBF data reading mode.
data ReadingMode
  = RawExtraction
  -- ^ Extract the entry file raw data as-is (compressed blocks are still
  -- compressed).
  | Decompression
  -- ^ Extract the entry file and decompress compressed data block on-the-fly.
  deriving (Eq, Show)

decodeBlockLen :: VBFBlockSize -> VBFSizeUnit
decodeBlockLen 0 = vbfBlockSize
decodeBlockLen l = fromIntegral l

-- | The vbfWithfExtractedEntry applies the given user function to the extracted
-- VBF enry file described by the function arguments.
-- Note that the entry data byte is represented as a lazy ByteString.
vbfWithfExtractedEntry
  :: FilePath
  -> VBFEntry
  -> EntryRange
  -> ReadingMode
  -> (BSL.ByteString -> IO a)
  -> IO a
vbfWithfExtractedEntry archive entry rg mode act =
  withBinaryFile archive ReadMode
    $ \hdl -> vbfReadEntryContentLazily hdl entry rg mode >>= act

-- | The vbfReadEntryContentLazily extracts the requested VBF entry file and
-- returns its content lazily.
vbfReadEntryContentLazily
  :: Handle -> VBFEntry -> EntryRange -> ReadingMode -> IO BSL.ByteString
vbfReadEntryContentLazily hdl entry rg RawExtraction = do
  hSeek hdl AbsoluteSeek actualVbfOffset
  BSL.hGet hdl (fromIntegral ctLen)
 where
  vbfEntryOff     = vbfeOffset entry
  maxLength       = vbfeCompressedSize entry
  (ctOff, ctLen)  = fixRange maxLength rg
  actualVbfOffset = fromIntegral (vbfEntryOff + ctOff)
vbfReadEntryContentLazily hdl entry rg Decompression = do
  archivedBlocks <- traverse fetchBlock blockInfos
  let processedBlocks = Vector.map (uncurry processBlock) archivedBlocks
  return
    $ BSL.take (fromIntegral ctLen)
    $ BSL.drop (fromIntegral skippedAlignedBytesCount)
    $ BSL.concat (Vector.toList processedBlocks)
 where
  maxLength                = vbfeSize entry
  (ctOff, ctLen)           = fixRange maxLength rg

  entryBlocks              = vbfeBlocks entry
  skippedBlkCount          = fromIntegral (ctOff `div` vbfBlockSize)
  skippedAlignedBytesCount = ctOff `rem` vbfBlockSize
  blkCount =
    fromIntegral $ vbfEntryBlockCount (skippedAlignedBytesCount + ctLen)
  blockInfos = Vector.slice skippedBlkCount blkCount entryBlocks

  fetchBlock (off, blk) = do
    hSeek hdl AbsoluteSeek (fromIntegral off)
    bs <- BSL.hGet hdl (fromIntegral $ vbfRawBlockSize vbfBlockSize blk)
    return (blk, bs)

  processBlock PassthroughBlock      bs = bs
  processBlock (PartialBlock    len) bs = (BSL.take (fromIntegral len) bs)
  processBlock (CompressedBlock _  ) bs = vbfBlockDecompress bs

fixRange :: VBFSizeUnit -> EntryRange -> (VBFSizeUnit, VBFSizeUnit)
fixRange maxLength EntireFile = (0, maxLength)
fixRange maxLength (PartialFile off bc) =
  (min off maxLength, min bc (maxLength - off))

-- | The vbfContent reads and returns the description of a VBF archive and its
-- contnt.
vbfContent :: FilePath -> IO VBFContent
vbfContent fp = withBinaryFile fp ReadMode go
 where
  go hdl = do
    vbfFileInfo <- vbfArchiveFileInfo hdl
    return $! toVBFContent vbfFileInfo

  toVBFContent fileInfo =
    let blocks    = vbfiBlocks fileInfo
        nameTable = vbfiNameTable fileInfo
        entries   = Vector.zipWith (toVBFEntry blocks nameTable)
                                   (vbfiEntries fileInfo)
                                   (vbfiHashes fileInfo)
    in  VBFContent { vbfcHeaderLength = vbfiHeaderLength fileInfo
                   , vbfcBlockCount   = fromIntegral (Vector.length blocks)
                   , vbfcEntries      = entries
                   }

  toVBFEntry blocks nameTable entry hash =
    let archivePath = BSL.takeWhile
          (/= '\NUL')
          (BSL.drop (fromIntegral $ vbffeNameOffset entry) nameTable)
        entryOffset       = vbffeOffset entry
        entryStartBlock   = vbffeStartBlock entry
        entryBlockCount   = vbfEntryBlockCount (vbffeBytes entry)
        entryBlockLengths = Vector.slice (fromIntegral entryStartBlock)
                                         (fromIntegral entryBlockCount)
                                         blocks
        entrySize         = vbffeBytes entry
        unalignedBlockLen = fromIntegral (entrySize `rem` vbfBlockSize)
        entryBlocks       = Vector.imap
          (toVBFBlock (fromIntegral $ entryBlockCount - 1) unalignedBlockLen)
          entryBlockLengths
        entryBlockOffsets =
            Vector.scanl' (+) entryOffset
              $   fromIntegral
              .   vbfRawBlockSize vbfBlockSize
              <$> entryBlocks
        entryBlocksWithOffset = Vector.zip entryBlockOffsets entryBlocks
    in  VBFEntry
          { vbfeOffset          = entryOffset
          , vbfeSize            = entrySize
          , vbfeCompressedSize  = sum (decodeBlockLen <$> entryBlockLengths)
          , vbfeArchivePath     = archivePath
          , vbfeArchivePathHash = hash
          , vbfeBlocks          = entryBlocksWithOffset
          }

  toVBFBlock :: Int -> VBFBlockSize -> Int -> VBFBlockSize -> VBFBlock
  toVBFBlock lstBlk unalBlkLen idx blkLen
    | blkLen == 0                           = PassthroughBlock
    | lstBlk == idx && blkLen == unalBlkLen = PartialBlock blkLen
    | otherwise                             = CompressedBlock blkLen
