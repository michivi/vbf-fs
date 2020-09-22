{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : System.FileSystem.VBF.Archive
-- Description : VBF archive file management
-----------------------------------------------------------------------------
module System.FileSystem.VBF.Archive
  (
  -- * VBF Archive
    VBFFileEntry(..)
  , VBFFileInfo(..)
  , VBFSignature
  , vbfArchiveFileInfo
  , vbfArchiveSignature
  , vbfBlockDecompress
  , vbfBlockSize
  , vbfEntryBlockCount
  , vbfHashBytes
  )
where

import           System.FileSystem.VBF.Data
import           System.FileSystem.VBF.Internal

import           Codec.Compression.Zlib
import           Control.Exception
import           Control.Monad
import qualified Crypto.Hash.MD5               as MD5
import           Data.Binary.Get
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BSL
import qualified Data.Vector                   as Vector
import           System.IO               hiding ( hGetContents )

data VBFFileInfo = VBFFileInfo
    { vbfiHeaderLength :: !VBFHeaderSize
    , vbfiEntries :: ![VBFFileEntry]
    , vbfiHashes :: ![VBFHash]
    , vbfiNameTable :: !BSL.ByteString
    , vbfiBlocks :: !(Vector.Vector VBFBlockSize) }

data VBFFileEntry = VBFFileEntry
    { vbffeStartBlock :: !VBFBlockIndex
    , vbffeBytes :: !VBFSizeUnit
    , vbffeOffset :: !VBFSizeUnit
    , vbffeNameOffset :: !VBFSizeUnit }

type VBFSignature = BS.ByteString

vbfArchiveSignature :: VBFSignature
vbfArchiveSignature = "SRYK"

vbfBlockSize :: VBFSizeUnit
vbfBlockSize = 65536

vbfBlockDecompress :: BSL.ByteString -> Either VBFBlockError BSL.ByteString
vbfBlockDecompress =
  either (const $ Left CorruptedCompressedBlock) Right . decompress

vbfHashBytes :: BSL.ByteString -> VBFHash
vbfHashBytes = MD5.hashlazy

vbfEntryBlockCount :: VBFSizeUnit -> VBFBlockIndex
vbfEntryBlockCount len =
  fromIntegral (len + vbfBlockSize - 1) `div` (fromIntegral vbfBlockSize)

vbfArchiveFileInfo :: Handle -> IO VBFFileInfo
vbfArchiveFileInfo hdl = do
  vbfFileInfo        <- readFileInfo
  computedHeaderHash <- computeHeaderHash (vbfiHeaderLength vbfFileInfo)
  storedHeaderHash   <- readHeaderHash
  when (computedHeaderHash /= storedHeaderHash) $ throwIO InvalidHeaderException
  return vbfFileInfo
 where
  readFileInfo = do
    hSeek hdl AbsoluteSeek 0
    content <- BSL.hGetContents hdl
    return $! runGet getVBFFileInfo content
  computeHeaderHash len = do
    hSeek hdl AbsoluteSeek 0
    header <- BSL.hGet hdl (fromIntegral len)
    return $! vbfHashBytes header
  readHeaderHash = do
    hSeek hdl SeekFromEnd (-16)
    hash <- BSL.hGet hdl 16
    return $! BSL.toStrict hash

  getSignature    = getByteString 4 :: Get VBFSignature
  getHeaderLength = fromIntegral <$> getWord32le :: Get VBFHeaderSize
  getNumFiles     = fromIntegral <$> getWord64le :: Get Int
  getHash         = getByteString 16 :: Get VBFHash
  getFileEntry    = do
    startBlock <- getWord32le
    skip 4
    bytes      <- getWord64le
    offset     <- getWord64le
    nameOffset <- getWord64le
    return $! VBFFileEntry { vbffeStartBlock = startBlock
                           , vbffeBytes      = bytes
                           , vbffeOffset     = offset
                           , vbffeNameOffset = nameOffset
                           }
  getNameTableLength = do
    nameTableLength <- getWord32le
    getLazyByteString (fromIntegral nameTableLength - 4)
  getBlock       = getWord16le
  getVBFFileInfo = do
    signature <- getSignature
    when (signature /= vbfArchiveSignature) $ throw InvalidSignatureException

    headerLength <- getHeaderLength
    numFiles     <- getNumFiles

    let minHeaderLength = fromIntegral (16 + (numFiles * 48) + 4)
    when (headerLength < minHeaderLength) $ throw InvalidHeaderException

    hashes      <- replicateM numFiles getHash
    fileEntries <- replicateM numFiles getFileEntry
    nameTable   <- getNameTableLength

    let blockCount =
          sum (fromIntegral . vbfEntryBlockCount . vbffeBytes <$> fileEntries)
    blocks <- Vector.replicateM blockCount getBlock

    return $! VBFFileInfo { vbfiHeaderLength = headerLength
                          , vbfiHashes       = hashes
                          , vbfiNameTable    = nameTable
                          , vbfiEntries      = fileEntries
                          , vbfiBlocks       = blocks
                          }
