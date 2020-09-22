-----------------------------------------------------------------------------
-- |
-- Module      : System.FileSystem.VBF.Internal
-- Description : VBF internal data types and functions
-----------------------------------------------------------------------------
module System.FileSystem.VBF.Internal
  (
  -- * VBF File system
    VBFBlock(..)
  , VBFContent(..)
  , VBFEntry(..)
  , vbfRawBlockSize

  -- * Error management
  , VBFBlockError(..)
  , VBFException(..)
  )
where

import           System.FileSystem.VBF.Data

import           Control.Exception
import           Data.Typeable
import           Data.Vector

data VBFBlock
    = CompressedBlock VBFBlockSize
    | PartialBlock VBFBlockSize
    | PassthroughBlock
    deriving (Eq, Show)

data VBFContent = VBFContent
    { vbfcBlockCount :: !VBFBlockIndex
    , vbfcEntries :: ![VBFEntry] }
    deriving (Eq, Show)

data VBFEntry = VBFEntry
    { vbfeOffset :: !VBFSizeUnit
    , vbfeSize :: !VBFSizeUnit
    , vbfeCompressedSize :: !VBFSizeUnit
    , vbfeArchivePath :: !FilePath
    , vbfeArchivePathHash :: !VBFHash
    , vbfeBlocks :: !(Vector (VBFSizeUnit, VBFBlock)) }
    deriving (Eq, Show)

data VBFException
    = InvalidSignatureException
    | InvalidHeaderException
    | InvalidBlockException VBFBlockError
    deriving (Eq, Show, Typeable)

data VBFBlockError
    = CorruptedCompressedBlock
    deriving (Eq, Show, Typeable)

instance Exception VBFException

vbfRawBlockSize :: VBFSizeUnit -> VBFBlock -> VBFSizeUnit
vbfRawBlockSize _   (CompressedBlock len) = fromIntegral len
vbfRawBlockSize _   (PartialBlock    len) = fromIntegral len
vbfRawBlockSize len PassthroughBlock      = len
