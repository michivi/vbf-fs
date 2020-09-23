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
  , vbfHashSize
  )
where

import           System.FileSystem.VBF.Data
import           System.FileSystem.VBF.Internal

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

data VBFFileInfo = VBFFileInfo
    { vbfiHeaderLength :: !VBFHeaderSize
    , vbfiEntries :: !(Vector.Vector VBFFileEntry)
    , vbfiHashes :: !(Vector.Vector VBFHash)
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

vbfBlockDecompress :: BSL.ByteString -> BSL.ByteString
vbfBlockDecompress = mapException toVBFException . decompress
 where
  toVBFException :: DecompressError -> VBFException
  toVBFException = const (InvalidBlockException CorruptedCompressedBlock)

vbfHashBytes :: BS.ByteString -> VBFHash
vbfHashBytes = MD5.hash

vbfHashSize :: Int
vbfHashSize = 16

vbfEntryBlockCount :: VBFSizeUnit -> VBFBlockIndex
vbfEntryBlockCount len =
  fromIntegral (len + vbfBlockSize - 1) `div` (fromIntegral vbfBlockSize)

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
