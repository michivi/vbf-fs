module Control.Effect.VBFFS.Tree
  ( VBFNodeTag(..)
  , VBFTree
  , vbfContentTree
  , vbfNodeName
  )
where

import           Control.Effect.VBFFS.Internal
import           Data.VBF

import           Control.Monad
import           Data.List
import qualified Data.List.NonEmpty            as NonEmpty
import           Data.Maybe
import           Data.Tree
import           System.FilePath.Posix

type VBFTree = Tree VBFNodeTag

data VBFNodeTag
    = VBFEntryTag String VBFHash
    | IntermediateTag String
    deriving (Eq, Ord, Show)

data VBFNodeBuildingBlock
    = VBFNodeBuildingBlock
        { vbfnbbContextRevPath :: NonEmpty.NonEmpty FilePath
        , vbfnbbLocalPath :: NonEmpty.NonEmpty FilePath
        , vbfnbbHash :: VBFHash }
    deriving (Eq, Ord, Show)

drillDownNode :: VBFNodeBuildingBlock -> Maybe VBFNodeBuildingBlock
drillDownNode (VBFNodeBuildingBlock (crp NonEmpty.:| crps) (a NonEmpty.:| as) hash)
  = (\nas -> VBFNodeBuildingBlock (a NonEmpty.:| crp : crps) nas hash)
    <$> NonEmpty.nonEmpty as

isLeafNode :: VBFNodeBuildingBlock -> Bool
isLeafNode (VBFNodeBuildingBlock _ (_ NonEmpty.:| as) _) = null as

vbfRootLabel :: String
vbfRootLabel = ""

vbfContentTree :: VBFContent -> VBFTree
vbfContentTree content = buildTree (vbfcEntries content)

buildTree :: [VBFEntry] -> VBFTree
buildTree ents = Node (IntermediateTag vbfRootLabel) (buildChildren entryPaths)
 where
  entryPaths = catMaybes
    (sort
      (   (   liftM2 (VBFNodeBuildingBlock (vbfRootLabel NonEmpty.:| []))
          <$> (NonEmpty.nonEmpty . splitDirectories . vbfeArchivePath)
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
      fileFromNode (VBFNodeBuildingBlock _ (n NonEmpty.:| _) h) =
        Node (VBFEntryTag n h) []
    in
      concat [nodeFromGroup <$> subGroups, fileFromNode <$> files]

vbfNodeName :: VBFNodeTag -> String
vbfNodeName (VBFEntryTag n _  ) = n
vbfNodeName (IntermediateTag n) = n
