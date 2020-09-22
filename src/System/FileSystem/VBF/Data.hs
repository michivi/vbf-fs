-----------------------------------------------------------------------------
-- |
-- Module      : System.FileSystem.VBF.Data
-- Description : VBF basic data types
-----------------------------------------------------------------------------
module System.FileSystem.VBF.Data where

import           Data.ByteString
import           Data.Word

type VBFBlockIndex = Word32
type VBFBlockSize = Word16

type VBFHeaderSize = Word32

type VBFSizeUnit = Word64

type VBFHash = ByteString
