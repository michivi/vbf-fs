{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
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

  -- * VBF archive creation
  , VBFFileEntryRequest(..)
  , vbfArchiveCreation
  )
where

import           System.FileSystem.VBF.Data

import           Codec.Compression.Zlib.Internal
import           Control.Exception
import           Control.Monad
import qualified Crypto.Hash.MD5               as MD5
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy          as BSL
import qualified Data.ByteString.Lazy.Internal as BSL
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

-- | A request to insert a VBF entry file in a VBF archive.
data VBFFileEntryRequest = VBFFileEntryRequest
    { vbfferFileSize :: VBFSizeUnit
    -- ^ Size in bytes of the file to insert.
    , vbfferPath :: FilePath }
    -- ^ Path of the file within the VBF archive.

data AccumulatedEntryRequest = AccumulatedEntryRequest
    { aerNameSectionOffset :: VBFSizeUnit
    , aerNextDataBlock :: VBFBlockIndex }
    deriving (Eq, Show)

instance Monoid AccumulatedEntryRequest where
  mempty = AccumulatedEntryRequest 0 0

instance Semigroup AccumulatedEntryRequest where
  a <> b = AccumulatedEntryRequest
    { aerNameSectionOffset = aerNameSectionOffset a + aerNameSectionOffset b
    , aerNextDataBlock     = aerNextDataBlock a + aerNextDataBlock b
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
vbfBlockDecompress :: BSL.ByteString -> Maybe BSL.ByteString
vbfBlockDecompress bs = mfilter (not . BSL.null) $ Just
  (foldDecompressStreamWithInput
    BSL.Chunk
    (const BSL.Empty)
    (const BSL.Empty)
    (decompressST zlibFormat defaultDecompressParams)
    bs
  )

-- | The 'vbfBlockCompress' function takes in an uncompressed data block and
-- returns its compressed version.
vbfBlockCompress :: BSL.ByteString -> BSL.ByteString
vbfBlockCompress = compress zlibFormat defaultCompressParams
  { compressLevel = bestCompression }

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

    pos    <- fromIntegral <$> bytesRead
    when (pos /= headerLength) $ throw (InvalidHeaderException pos headerLength)

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

-- | The 'vbfArchiveCreation' creates a new VBF archive inside the specified
-- file handle.
vbfArchiveCreation
  :: Handle
  -> [a]
  -> (a -> VBFFileEntryRequest)
  -> (a -> IO BSL.ByteString)
  -> IO ()
vbfArchiveCreation hdl as freq fdat = do
  writtenBlocks <- writeDataBlocks
  hdr           <- writeHeader writtenBlocks
  writeHeaderSignature hdr
 where
  vas           = Vector.fromList as
  numFiles      = Vector.length vas
  entryRequests = freq <$> vas
  aers'         = Vector.scanl (<>) mempty (toAer <$> entryRequests)
  aers          = Vector.init aers'
  accAers       = Vector.last aers'
  toAer (VBFFileEntryRequest bc fp) = AccumulatedEntryRequest
    { aerNameSectionOffset = fromIntegral (length fp + 1)
    , aerNextDataBlock     = vbfEntryBlockCount bc
    }
  names        = vbfferPath <$> entryRequests
  nameTableLen = aerNameSectionOffset accAers
  headerLen =
    16 + (numFiles * 48) + 4 + fromIntegral nameTableLen + fromIntegral
      (2 * aerNextDataBlock accAers)
  writeDataBlocks = do
    hSeek hdl AbsoluteSeek (fromIntegral headerLen)
    Vector.zipWithM writeEntryDataBlocks vas entryRequests
  writeEntryDataBlocks a (VBFFileEntryRequest bc _) = do
    dat <- BSL.take (fromIntegral bc) <$> fdat a
    let blks           = Vector.unfoldr splitBlock dat
        compressedBlks = processSingleBlock <$> blks

    forM_ (snd <$> compressedBlks) $ BSL.hPut hdl

    when (BSL.length dat /= fromIntegral bc)
      $ throwIO IncorrectInputSizeException

    return $ fst <$> compressedBlks
  splitBlock (BSL.null -> True) = Nothing
  splitBlock bs = Just $ BSL.splitAt (fromIntegral vbfBlockSize) bs
  processSingleBlock bs =
    let compressed      = vbfBlockCompress bs
        compressedLen   = BSL.length compressed
        uncompressedLen = BSL.length bs
        blkSize         = fromIntegral vbfBlockSize
    in  case (compressedLen, uncompressedLen) of
          (c, u) | c < u       -> (CompressedBlock (fromIntegral c), compressed)
          (_, u) | u < blkSize -> (PartialBlock (fromIntegral u), bs)
          _                    -> (PassthroughBlock, bs)
  writeHeader blks = do
    hSeek hdl AbsoluteSeek 0
    let hdr = runPut (putHeader blks)
    BSL.hPut hdl hdr
    return $ BSL.toStrict hdr
  writeHeaderSignature hdr = do
    let hash = vbfHashBytes hdr
    hSeek hdl SeekFromEnd 0
    BS.hPut hdl hash
  putHeader blks = do
    putByteString vbfArchiveSignature
    putWord32le $ fromIntegral headerLen
    putWord64le $ fromIntegral numFiles
    let doffs = Vector.scanl (+) (fromIntegral headerLen) (sum . fmap (vbfRawBlockSize vbfBlockSize) <$> blks)
    forM_ (vbfHashBytes . BS.pack . vbfferPath <$> entryRequests) putByteString
    Vector.zipWithM_ putEntry doffs (Vector.zip aers entryRequests)
    putWord32le $ fromIntegral nameTableLen + 4
    forM_ names $ \name -> putByteString (BS.pack name) *> putWord8 0
    forM_ (join blks) putBlock
  putEntry doff (AccumulatedEntryRequest nso dblk, VBFFileEntryRequest bc _) =
    do
      putWord32le dblk
      putWord32le 0
      putWord64le bc
      putWord64le doff
      putWord64le nso
  putBlock (CompressedBlock bc) = putWord16le bc
  putBlock (PartialBlock    bc) = putWord16le bc
  putBlock PassthroughBlock     = putWord16le 0

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
