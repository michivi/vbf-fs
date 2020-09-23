{-# LANGUAGE OverloadedStrings #-}
module System.FileSystem.VBF.Tree
  ( VBFNodeTag(..)
  , VBFTree
  , vbfContentTree
  , vbfNodeName
  , vbfNodeTagName
  )
where

import           System.FileSystem.VBF.Data
import           System.FileSystem.VBF.Internal

import qualified Data.ByteString.Lazy.Char8    as BSL
import           Data.List
import qualified Data.List.NonEmpty            as NonEmpty
import           Data.Tree
import qualified Data.Vector                   as Vector

type VBFTree = Tree VBFNodeTag

data VBFNodeTag
    = VBFEntryTag !BSL.ByteString !VBFSizeUnit !VBFHash
    | IntermediateTag !BSL.ByteString
    deriving (Eq, Show)

data BuildingPiece
  = BranchPiece !VBFEntry !BSL.ByteString !BSL.ByteString
  | LeafPiece !VBFEntry !BSL.ByteString
    deriving (Eq, Show)

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

vbfNodeName :: VBFTree -> String
vbfNodeName (Node tag _) = vbfNodeTagName tag

vbfNodeTagName :: VBFNodeTag -> String
vbfNodeTagName (VBFEntryTag n _ _) = BSL.unpack n
vbfNodeTagName (IntermediateTag n) = BSL.unpack n
