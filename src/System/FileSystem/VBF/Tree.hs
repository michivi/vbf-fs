{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : System.FileSystem.VBF.Tree
-- Description : VBF filename tree
--
-- This module exposes various functions that can be used to build a tree
-- with a VBF archive entries. Each leaf represents a VBF file entry with its
-- associated hash value.
-----------------------------------------------------------------------------
module System.FileSystem.VBF.Tree
  ( VBFNodeTag(..)
  , VBFTree
  , vbfContentTree
  , vbfNodeName
  , vbfNodeTagName
  , vbfRootLabel
  )
where

import           System.FileSystem.VBF.Data

import qualified Data.ByteString.Lazy.Char8    as BSL
import           Data.List
import qualified Data.List.NonEmpty            as NonEmpty
import           Data.Tree
import qualified Data.Vector                   as Vector

-- | A VBF name tree. Each node is associated with a VBFNodeTag.
type VBFTree = Tree VBFNodeTag

-- | Tag indicating the node function in the tree.
data VBFNodeTag
    = VBFEntryTag !BSL.ByteString !VBFSizeUnit !VBFHash
    -- ^ The node represents a VBF file entry with the given name, size in bytes
    -- and path hash code.
    | IntermediateTag !BSL.ByteString
    -- ^ The node is used in the VBF tree as a folder but has no real
    -- representation in the VBF archive file.
    deriving (Eq, Show)

data BuildingPiece
  = BranchPiece !VBFEntry !BSL.ByteString !BSL.ByteString
  | LeafPiece !VBFEntry !BSL.ByteString
    deriving (Eq, Show)

-- | Label associated with the root node of the VBF name tree.
vbfRootLabel :: BSL.ByteString
vbfRootLabel = ""

toBuildingPiece :: BSL.ByteString -> VBFEntry -> BuildingPiece
toBuildingPiece relPath e = matchPiece cur nxt
 where
  (cur, nxt) = BSL.break (== vbfPathSeparator) relPath
  matchPiece c n | BSL.length nxt < 1 = LeafPiece e c
                 | otherwise          = BranchPiece e c (BSL.drop 1 n)

pieceName :: BuildingPiece -> BSL.ByteString
pieceName (BranchPiece _ c _) = c
pieceName (LeafPiece _ c    ) = c

pieceSubPath :: BuildingPiece -> BSL.ByteString
pieceSubPath (BranchPiece _ _ n) = n
pieceSubPath (LeafPiece _ _    ) = BSL.empty

pieceEntry :: BuildingPiece -> VBFEntry
pieceEntry (BranchPiece e _ _) = e
pieceEntry (LeafPiece e _    ) = e

-- | The vbfContentTree returns a VBF name tree for the specified VBF archive
-- description.
vbfContentTree :: VBFContent -> VBFTree
vbfContentTree content = buildTree (Vector.toList $ vbfcEntries content)

buildTree :: [VBFEntry] -> VBFTree
buildTree ents = Node (IntermediateTag vbfRootLabel)
                      (buildChildren sortedBBlocks)
 where
  sortedBBlocks =
    (toBuildingPiece <$> vbfeArchivePath <*> id) <$> sortOn vbfeArchivePath ents

  buildChildren sbbs =
    let subGroups = NonEmpty.groupWith pieceName sbbs
        toTree ((LeafPiece e n) NonEmpty.:| _) =
            Node (VBFEntryTag n (vbfeSize e) (vbfeArchivePathHash e)) []
        toTree dirs@((BranchPiece _ n _) NonEmpty.:| _) =
            Node (IntermediateTag n) $ buildChildren
              (NonEmpty.toList
                ((toBuildingPiece <$> pieceSubPath <*> pieceEntry) <$> dirs)
              )
    in  toTree <$> subGroups

-- | The vbfNodeName function returns the node's file or folder name.
vbfNodeName :: VBFTree -> String
vbfNodeName (Node tag _) = vbfNodeTagName tag

-- | The vbfNodeTagName function returns the name of the file or folder
-- represented by the given tag.
vbfNodeTagName :: VBFNodeTag -> String
vbfNodeTagName (VBFEntryTag n _ _) = BSL.unpack n
vbfNodeTagName (IntermediateTag n) = BSL.unpack n
