{-# LANGUAGE OverloadedStrings #-}
module Control.Effect.VBFFS.Internal where

import           Data.VBF

import           Control.Exception
import           Data.ByteString                ( ByteString )
import           Data.Typeable

data VBFContent = VBFContent
    { vbfcEntries :: ![VBFEntry] }
    deriving (Eq, Show)

data VBFEntry = VBFEntry
    { vbfeOffset :: !VBFSizeUnit
    , vbfeSize :: !VBFSizeUnit
    , vbfeArchivePath :: !FilePath
    , vbfeArchivePathHash :: !VBFHash }
    deriving (Eq, Show)

data VBFFileInfo = VBFFileInfo
    { vbfiHeaderLength :: !VBFHeaderSize
    , vbfiEntries :: ![VBFFileEntry]
    , vbfiHashes :: ![VBFHash]
    , vbfiNameTable :: !ByteString }

data VBFFileEntry = VBFFileEntry
    { vbffeStartBlock :: !VBFBlockIndex
    , vbffeBytes :: !VBFSizeUnit
    , vbffeOffset :: !VBFSizeUnit
    , vbffeNameOffset :: !VBFSizeUnit }

data VBFException
    = InvalidSignatureException
    | InvalidHeaderException
    deriving (Show, Typeable)

instance Exception VBFException

type VBFSignature = ByteString

vbfArchiveSignature :: VBFSignature
vbfArchiveSignature = "SRYK"

vbfBlockSize :: VBFSizeUnit
vbfBlockSize = 65536
