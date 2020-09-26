-----------------------------------------------------------------------------
-- |
-- Module      : System.FileSystem.VBF.Data
-- Description : VBF basic data types
-----------------------------------------------------------------------------
module System.FileSystem.VBF.Data
  (
  -- * Common data types and functions
    VBFBlockIndex
  , VBFBlockSize
  , VBFHeaderSize
  , VBFSizeUnit
  , VBFHash
  , VBFSignature
  , vbfPathSeparator

  -- * VBF archive description
  , VBFBlock(..)
  , VBFContent(..)
  , VBFEntry(..)

  -- * Error management
  , VBFBlockError(..)
  , VBFException(..)
  )
where

import           Control.Exception
import           Data.ByteString
import qualified Data.ByteString.Lazy          as BSL
import           Data.Typeable
import           Data.Vector
import           Data.Word

-- | Type used to represent the index of a data blocks in a VBF entry
-- description.
type VBFBlockIndex = Word32

-- | Type used to represent the size of a data blocks in a VBF entry
-- description in bytes.
type VBFBlockSize = Word16

-- | Type for the VBF archive file header size in bytes.
type VBFHeaderSize = Word32

-- | Type for any VBF entry file size in bytes.
type VBFSizeUnit = Word64

-- | Type for a VBF entry path hash value.
type VBFHash = ByteString

-- | Type for the VBF file signature.
type VBFSignature = ByteString

-- | Type of block within a VBF file.
data VBFBlock
    = CompressedBlock !VBFBlockSize
    -- ^ A compressed block with the associated compressed size in bytes.
    | PartialBlock !VBFBlockSize
    -- ^ An uncompressed block which content has the specified number of bytes.
    | PassthroughBlock
    -- ^ A full uncompressed block.
    deriving (Eq, Show)

-- | Description of a VBF archive.
data VBFContent = VBFContent
    { vbfcHeaderLength :: !VBFHeaderSize
    -- ^ Size in bytes of the header in the VBF archive.
    , vbfcBlockCount :: !VBFBlockIndex
    -- ^ Total number of data blocks in the VBF archive.
    , vbfcEntries :: (Vector VBFEntry) }
    -- ^ Entries within the VBF archive.
    deriving (Eq, Show)

-- | An entry within the VBF archive (that is a single file).
data VBFEntry = VBFEntry
    { vbfeOffset :: !VBFSizeUnit
    -- ^ Offset to the entry's first block of data in the VBF archive file.
    , vbfeSize :: !VBFSizeUnit
    -- ^ Uncompressed entry file size in bytes.
    , vbfeCompressedSize :: VBFSizeUnit
    -- ^ Compressed entry file size in bytes.
    , vbfeArchivePath :: !BSL.ByteString
    -- ^ Full path to the entry file within the VBF archive.
    , vbfeArchivePathHash :: !VBFHash
    -- ^ Hash value of the entry path within the VBF archive.
    , vbfeBlocks :: (Vector (VBFSizeUnit, VBFBlock)) }
    -- ^ The entry file's data blocks.
    deriving (Eq, Show)

-- | Exceptions that might be thrown during VBF file handling.
data VBFException
    = InvalidSignatureException
    -- ^ The VBF archive has an invalid header signature.
    | CorruptedHeaderException
    -- ^ The VBF file header might have been corrupted (checksums don't match).
    | HeaderTooSmallException
    -- ^ The VBF file header is smaller than expected.
    | InvalidHeaderException
    -- ^ The VBF file header doesn't have the expected size.
    | InvalidBlockException VBFBlockError
    -- ^ A VBF data block could not be processed.
    deriving (Eq, Show, Typeable)

-- | Data block processing error.
data VBFBlockError
    = CorruptedCompressedBlock
    -- ^ The compressed data block is corrupted and cannot be decompressed.
    deriving (Eq, Show, Typeable)

instance Exception VBFException

-- | Path separator used in VBF archive paths.
vbfPathSeparator :: Char
vbfPathSeparator = '/'
