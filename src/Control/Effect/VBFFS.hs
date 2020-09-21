{-# LANGUAGE OverloadedStrings #-}
module Control.Effect.VBFFS
  ( ExtractionMode(..)
  , vbfContent
  , vbfExtractEntry
  , vbfUncompressedEntrySize
  )
where

import           Control.Effect.VBFFS.Internal
import           Data.VBF

import           Codec.Compression.Zlib
import           Control.Exception
import           Control.Monad
import qualified Crypto.Hash.MD5               as MD5
import           Data.Binary.Get
import qualified Data.ByteString.Lazy.Char8    as BSL
import qualified Data.Vector
import           System.IO               hiding ( hGetContents )

data ExtractionMode = RawExtraction | Decompression deriving (Eq, Show)

decodeBlockLen :: VBFBlockSize -> VBFSizeUnit
decodeBlockLen 0 = vbfBlockSize
decodeBlockLen l = fromIntegral l

vbfUncompressedEntrySize :: VBFEntry -> VBFSizeUnit
vbfUncompressedEntrySize e = sum (decodeBlockLen <$> vbfeBlocks e)

vbfExtractEntry
  :: FilePath -> VBFEntry -> ExtractionMode -> (BSL.ByteString -> IO a) -> IO a
vbfExtractEntry archive entry mode act =
  withBinaryFile archive ReadMode $ \hdl -> do
    hSeek hdl AbsoluteSeek (fromIntegral $ vbfeOffset entry)

    archivedBlocks <- traverse
      (BSL.hGet hdl)
      (fromIntegral . decodeBlockLen <$> vbfeBlocks entry)

    let lastUncompressedBlockLen = fromIntegral
          (decodeBlockLen $ (fromIntegral $ vbfeSize entry `rem` vbfBlockSize))

    case mode of
      RawExtraction -> act (BSL.concat $ Data.Vector.toList archivedBlocks)
      Decompression ->
        let decompressedBlocks = Data.Vector.imapM
              (decompressEntry (Data.Vector.length archivedBlocks - 1)
                               lastUncompressedBlockLen
              )
              archivedBlocks
        in  do
              decomp <- either throwIO return decompressedBlocks
              act (BSL.concat $ Data.Vector.toList decomp)
 where
  decompressEntry lst unc idx blk
    | BSL.length blk == fromIntegral vbfBlockSize = Right blk
    | idx == lst && BSL.length blk == unc = Right blk
    | otherwise                           = decompress blk

vbfContent :: FilePath -> IO VBFContent
vbfContent fp = withBinaryFile fp ReadMode go
 where
  go hdl = do
    vbfFileInfo        <- readVBFFileInfo hdl
    computedHeaderHash <- computeHeaderHash hdl (vbfiHeaderLength vbfFileInfo)
    storedHeaderHash   <- readHeaderHash hdl
    when (computedHeaderHash /= storedHeaderHash)
      $ throwIO InvalidHeaderException
    return $! toVBFContent vbfFileInfo

  readVBFFileInfo hdl = do
    hSeek hdl AbsoluteSeek 0
    content <- BSL.hGetContents hdl
    return $! runGet getVBFFileInfo content
  computeHeaderHash hdl len = do
    hSeek hdl AbsoluteSeek 0
    header <- BSL.hGet hdl (fromIntegral len)
    return $! MD5.hashlazy header
  readHeaderHash hdl = do
    hSeek hdl SeekFromEnd (-16)
    hash <- BSL.hGet hdl 16
    return $! BSL.toStrict hash

  entryBlockCount len =
    fromIntegral (len + vbfBlockSize - 1) `div` (fromIntegral vbfBlockSize)
  toVBFContent fileInfo =
    let
      entries = zipWith
        (toVBFEntry (vbfiNameTable fileInfo) (vbfiBlocks fileInfo))
        (vbfiEntries fileInfo)
        (vbfiHashes fileInfo)
      toVBFEntry nameTable blocks entry hash = VBFEntry
        { vbfeOffset          = vbffeOffset entry
        , vbfeSize            = vbffeBytes entry
        , vbfeArchivePath     = BSL.unpack $ BSL.takeWhile
                                  (/= '\NUL')
                                  (BSL.drop (fromIntegral $ vbffeNameOffset entry)
                                            (BSL.fromStrict nameTable)
                                  )
        , vbfeArchivePathHash = hash
        , vbfeBlocks          = Data.Vector.slice
                                  (fromIntegral (vbffeStartBlock entry))
                                  (entryBlockCount (vbffeBytes entry))
                                  blocks
        }
    in
      VBFContent { vbfcEntries = entries }

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
    getByteString (fromIntegral nameTableLength - 4)
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

    let blockCount = sum (entryBlockCount . vbffeBytes <$> fileEntries)
    blocks <- Data.Vector.replicateM blockCount getBlock

    return $! VBFFileInfo { vbfiHeaderLength = headerLength
                          , vbfiHashes       = hashes
                          , vbfiNameTable    = nameTable
                          , vbfiEntries      = fileEntries
                          , vbfiBlocks       = blocks
                          }

