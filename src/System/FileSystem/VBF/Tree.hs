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

type VBFTree = Tree VBFNodeTag

data VBFNodeTag
    = VBFEntryTag !BSL.ByteString !VBFSizeUnit !VBFHash
    | IntermediateTag !BSL.ByteString
    deriving (Eq, Show)

data VBFNodeBuildingBlock
  = DirectoryBlock !VBFEntry !BSL.ByteString !BSL.ByteString
  | FileBlock !VBFEntry !BSL.ByteString
    deriving (Eq, Show)

vbfRootLabel :: BSL.ByteString
vbfRootLabel = ""

toBuildingBlock :: BSL.ByteString -> VBFEntry -> VBFNodeBuildingBlock
toBuildingBlock relPath e = matchBlock cur nxt
 where
  (cur, nxt) = BSL.span (/= vbfPathSeparator) relPath
  matchBlock c n | BSL.length nxt < 1 = FileBlock e c
                 | otherwise          = DirectoryBlock e c (BSL.drop 1 n)

buildingBlockName :: VBFNodeBuildingBlock -> BSL.ByteString
buildingBlockName (DirectoryBlock _ c _) = c
buildingBlockName (FileBlock _ c       ) = c

buildingBlockSubPath :: VBFNodeBuildingBlock -> BSL.ByteString
buildingBlockSubPath (DirectoryBlock _ _ n) = n
buildingBlockSubPath (FileBlock _ _       ) = BSL.empty

buildingBlockEntry :: VBFNodeBuildingBlock -> VBFEntry
buildingBlockEntry (DirectoryBlock e _ _) = e
buildingBlockEntry (FileBlock e _       ) = e

vbfContentTree :: VBFContent -> VBFTree
vbfContentTree content = buildTree (vbfcEntries content)

buildTree :: [VBFEntry] -> VBFTree
buildTree ents = Node (IntermediateTag vbfRootLabel)
                      (buildChildren sortedBBlocks)
 where
  sortedBBlocks =
    (toBuildingBlock <$> vbfeArchivePath <*> id) <$> sortOn vbfeArchivePath ents

  buildChildren sbbs =
    let
      subGroups = NonEmpty.groupWith buildingBlockName sbbs
      toTree ((FileBlock e n) NonEmpty.:| _) =
        Node (VBFEntryTag n (vbfeSize e) (vbfeArchivePathHash e)) []
      toTree dirs@((DirectoryBlock _ n _) NonEmpty.:| _) =
        Node (IntermediateTag n) $ buildChildren
          (NonEmpty.toList
            ((toBuildingBlock <$> buildingBlockSubPath <*> buildingBlockEntry)
            <$> dirs
            )
          )
    in
      toTree <$> subGroups

vbfNodeName :: VBFTree -> String
vbfNodeName (Node tag _) = vbfNodeTagName tag

vbfNodeTagName :: VBFNodeTag -> String
vbfNodeTagName (VBFEntryTag n _ _) = BSL.unpack n
vbfNodeTagName (IntermediateTag n) = BSL.unpack n
