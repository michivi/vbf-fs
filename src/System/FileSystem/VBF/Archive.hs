{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : System.FileSystem.VBF.Archive
-- Description : VBF archive file management
--
-- This module exposes functions that relate to the handling of VBF archive
-- files.
-----------------------------------------------------------------------------
module System.FileSystem.VBF.Archive
  (
  -- * VBF archive file

  -- $vbfArchive
    VBFFileInfo(..)
  , vbfArchiveFileInfo
  , vbfArchiveSignature

  -- * VBF file entry
  , VBFFileEntry(..)

  -- * VBF block
  , vbfBlockSize
  , vbfBlockDecompress
  , vbfEntryBlockCount
  , vbfRawBlockSize

  -- * VBF common functions
  , vbfHashBytes
  , vbfHashSize
  )
where

import           System.FileSystem.VBF.Data

import           Codec.Compression.Zlib
import           Codec.Compression.Zlib.Internal
                                                ( DecompressError )
import           Control.Exception
import           Control.Monad
import qualified Crypto.Hash.MD5               as MD5
import           Data.Binary.Get
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BSL
import qualified Data.Vector                   as Vector
import           System.IO               hiding ( hGetContents )

-- | A VBF archive file (.vbf) header data.
data VBFFileInfo = VBFFileInfo
    { vbfiHeaderLength :: !VBFHeaderSize
    -- ^ Size of the header in bytes.
    , vbfiEntries :: !(Vector.Vector VBFFileEntry)
    -- ^ VBF file entries within the archive file.
    , vbfiHashes :: !(Vector.Vector VBFHash)
    -- ^ VBF file hashed paths within the archive file.
    , vbfiNameTable :: !BSL.ByteString
    -- ^ VBF name table, containing the raw file paths.
    , vbfiBlocks :: !(Vector.Vector VBFBlockSize)
    -- ^ VBF file data block sizes in bytes.
     }

-- | A file entry within a VBF archive file.
data VBFFileEntry = VBFFileEntry
    { vbffeStartBlock :: !VBFBlockIndex
    -- ^ Index of the first block of data in the VBF archive file. 
    -- All the remaining data blocks will follow the first block.
    , vbffeBytes :: !VBFSizeUnit
    -- ^ Uncompressed entry file size in bytes.
    , vbffeOffset :: !VBFSizeUnit
    -- ^ Offset to the entry's first block of data in the VBF archive file.
    , vbffeNameOffset :: !VBFSizeUnit
    -- ^ Offset of the file path within the name table in bytes.
     }

-- | VBF archive signature.
vbfArchiveSignature :: VBFSignature
vbfArchiveSignature = "SRYK"

-- | VBF archive data block size in bytes.
vbfBlockSize :: VBFSizeUnit
vbfBlockSize = 65536

-- | The 'vbfBlockDecompress' function takes in a compressed data block and
-- returns its decompressed version.
--
-- Failures are thrown 'InvalidBlockException' exceptions.
vbfBlockDecompress :: BSL.ByteString -> BSL.ByteString
vbfBlockDecompress = mapException toVBFException . decompress
 where
  toVBFException :: DecompressError -> VBFException
  toVBFException = const (InvalidBlockException CorruptedCompressedBlock)

-- | The 'vbfHashBytes' computes the hash value of the given data.
vbfHashBytes :: BS.ByteString -> VBFHash
vbfHashBytes = MD5.hash

-- | Size of a VBF hash value in bytes.
vbfHashSize :: Int
vbfHashSize = 16

-- | The 'vbfEntryBlockCount' computes the size of an entry in data blocks.
vbfEntryBlockCount :: VBFSizeUnit -> VBFBlockIndex
vbfEntryBlockCount len =
  fromIntegral (len + vbfBlockSize - 1) `div` (fromIntegral vbfBlockSize)

-- | The 'vbfArchiveFileInfo' returns the description of a VBF archive file
-- using the specified handle.
--
-- @
-- processed = withBinaryFile fp ReadMode doSomething
-- 
-- doSomething hdl = vbfArchiveFileInfo hdl >>= process
-- @
vbfArchiveFileInfo :: Handle -> IO VBFFileInfo
vbfArchiveFileInfo hdl = readHeaderHash >>= readAndValidateFileInfo
 where
  readHeaderHash = do
    hSeek hdl SeekFromEnd (fromIntegral $ -vbfHashSize)
    hash <- BS.hGet hdl vbfHashSize
    return hash
  readAndValidateFileInfo expectedHash = do
    hSeek hdl AbsoluteSeek 0
    content <- BSL.hGetContents hdl
    return $! runGet (getValidatedFileInfo expectedHash) content

  getValidatedFileInfo expectedHash = do
    (signature, hdrLen) <- lookAhead getSignatureAndLength
    when (signature /= vbfArchiveSignature) $ throw InvalidSignatureException

    header <- lookAhead $ getByteString (fromIntegral hdrLen)
    when (vbfHashBytes header /= expectedHash) $ throw CorruptedHeaderException

    getVBFFileInfo

  getSignature          = getByteString 4 :: Get VBFSignature
  getHeaderLength       = fromIntegral <$> getWord32le :: Get VBFHeaderSize
  getSignatureAndLength = do
    signature <- getSignature
    len       <- getHeaderLength
    return (signature, len)
  getNumFiles  = fromIntegral <$> getWord64le :: Get Int
  getHash      = getByteString vbfHashSize :: Get VBFHash
  getFileEntry = do
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
  getNameTable = do
    nameTableLength <- getWord32le
    getLazyByteString (fromIntegral nameTableLength - 4)
  getBlock       = getWord16le
  getVBFFileInfo = do
    (_, headerLength) <- getSignatureAndLength
    numFiles          <- getNumFiles

    let minHeaderLength = fromIntegral (16 + (numFiles * 48) + 4)
    when (headerLength < minHeaderLength) $ throw HeaderTooSmallException

    hashes      <- Vector.replicateM numFiles getHash
    fileEntries <- Vector.replicateM numFiles getFileEntry
    nameTable   <- getNameTable

    let blockCount =
          sum (fromIntegral . vbfEntryBlockCount . vbffeBytes <$> fileEntries)
    blocks <- Vector.replicateM blockCount getBlock

    pos    <- bytesRead
    when (fromIntegral pos /= headerLength) $ throw InvalidHeaderException

    return $! VBFFileInfo { vbfiHeaderLength = headerLength
                          , vbfiHashes       = hashes
                          , vbfiNameTable    = nameTable
                          , vbfiEntries      = fileEntries
                          , vbfiBlocks       = blocks
                          }

-- | The 'vbfRawBlockSize' function takes in a VBF data block and returns its
-- real size in bytes within the archive. The first parameter is the VBF archive
-- block size in bytes.
vbfRawBlockSize :: VBFSizeUnit -> VBFBlock -> VBFSizeUnit
vbfRawBlockSize _   (CompressedBlock len) = fromIntegral len
vbfRawBlockSize _   (PartialBlock    len) = fromIntegral len
vbfRawBlockSize len PassthroughBlock      = len

-- $vbfArchive
--
-- A VBF archive file contains the three following sections:
--
--   1. The header
--   2. The data blocks
--   3. The hash code of the header
--
-- The header itself (represented by 'VBFFileInfo') contains the three following
-- main sections:
--
--   1. A hash table, containing the hash code of each file path
--   2. An entry table, each entry representing a file (see 'VBFFileEntry')
--   3. A name table, containing the full path of each file
--   4. A table of 16 bits integer, representing the size of each data block
--
-- Each data block can be compressed individually.
--
