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

import           Control.Monad
import           Data.List
import qualified Data.List.NonEmpty            as NonEmpty
import           Data.Maybe
import           Data.Tree
import           System.FilePath.Posix

type VBFTree = Tree VBFNodeTag

data VBFNodeTag
    = VBFEntryTag String VBFSizeUnit VBFHash
    | IntermediateTag String
    deriving (Eq, Ord, Show)

data VBFNodeBuildingBlock
    = VBFNodeBuildingBlock
        { vbfnbbContextRevPath :: NonEmpty.NonEmpty FilePath
        , vbfnbbLocalPath :: NonEmpty.NonEmpty FilePath
        , vbfnbbLength :: VBFSizeUnit
        , vbfnbbHash :: VBFHash }
    deriving (Eq, Ord, Show)

drillDownNode :: VBFNodeBuildingBlock -> Maybe VBFNodeBuildingBlock
drillDownNode (VBFNodeBuildingBlock (crp NonEmpty.:| crps) (a NonEmpty.:| as) len hash)
  = (\nas -> VBFNodeBuildingBlock (a NonEmpty.:| crp : crps) nas len hash)
    <$> NonEmpty.nonEmpty as

isLeafNode :: VBFNodeBuildingBlock -> Bool
isLeafNode (VBFNodeBuildingBlock _ (_ NonEmpty.:| as) _ _) = null as

vbfRootLabel :: String
vbfRootLabel = ""

vbfContentTree :: VBFContent -> VBFTree
vbfContentTree content = buildTree (vbfcEntries content)

buildTree :: [VBFEntry] -> VBFTree
buildTree ents = Node (IntermediateTag vbfRootLabel) (buildChildren entryPaths)
 where
  entryPaths = catMaybes
    (sort
      (   (   liftM3 (VBFNodeBuildingBlock (vbfRootLabel NonEmpty.:| []))
          <$> (NonEmpty.nonEmpty . splitDirectories . vbfeArchivePath)
          <*> (Just . vbfeSize)
          <*> (Just . vbfeArchivePathHash)
          )
      <$> ents
      )
    )
  buildChildren sps =
    let
      subGroups = NonEmpty.groupWith (NonEmpty.head . vbfnbbContextRevPath)
                                     (mapMaybe drillDownNode sps)
      files = filter isLeafNode sps
      nodeFromGroup grp = Node
        (IntermediateTag . NonEmpty.head . vbfnbbContextRevPath $ NonEmpty.head
          grp
        )
        (buildChildren $ NonEmpty.toList grp)
      fileFromNode (VBFNodeBuildingBlock _ (n NonEmpty.:| _) len h) =
        Node (VBFEntryTag n len h) []
    in
      concat [nodeFromGroup <$> subGroups, fileFromNode <$> files]

vbfNodeName :: VBFTree -> String
vbfNodeName (Node tag _) = vbfNodeTagName tag

vbfNodeTagName :: VBFNodeTag -> String
vbfNodeTagName (VBFEntryTag n _ _) = n
vbfNodeTagName (IntermediateTag n) = n
