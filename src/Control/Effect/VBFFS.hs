module Control.Effect.VBFFS where

import           Control.Effect.VBFFS.Internal
import           Data.VBF

import           Control.Exception
import           Control.Monad
import qualified Crypto.Hash.MD5               as MD5
import           Data.Binary.Get
import qualified Data.ByteString.Lazy.Char8    as BSL
import           System.IO               hiding ( hGetContents )

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

  toVBFContent fileInfo =
    let
      entries = zipWith (toVBFEntry (vbfiNameTable fileInfo))
                        (vbfiEntries fileInfo)
                        (vbfiHashes fileInfo)
      toVBFEntry nameTable entry hash = VBFEntry
        { vbfeOffset          = vbffeOffset entry
        , vbfeSize            = vbffeBytes entry
        , vbfeArchivePath     = BSL.unpack $ BSL.takeWhile
                                  (/= '\NUL')
                                  (BSL.drop (fromIntegral $ vbffeNameOffset entry)
                                            (BSL.fromStrict nameTable)
                                  )
        , vbfeArchivePathHash = hash
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
    getByteString (fromIntegral nameTableLength)
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

    return $! VBFFileInfo { vbfiHeaderLength = headerLength
                          , vbfiHashes       = hashes
                          , vbfiNameTable    = nameTable
                          , vbfiEntries      = fileEntries
                          }

