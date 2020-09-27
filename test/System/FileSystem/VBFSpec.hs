module System.FileSystem.VBFSpec
  ( spec
  )
where

import           System.FileSystem.VBF

import           Data.List
import           System.FilePath
import           Test.Hspec
import           Test.QuickCheck

genVbfNameChar :: Gen Char
genVbfNameChar =
  elements (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ ".,;&#()-_@+")

genVbfName :: Gen String
genVbfName = listOf1 genVbfNameChar `suchThat` isNotReserved
 where
  isNotReserved "." = False
  isNotReserved _   = True

genVbfPath :: Gen String -> Gen String
genVbfPath nameGen = intercalate (vbfPathSeparator : []) <$> listOf1 nameGen

spec :: Spec
spec = describe "System.FileSystem.VBFSpec" $ do
  describe "osPathToVbfPath" $ do
    it "returns Nothing if the path is empty"
      $          osPathToVbfPath ""
      `shouldBe` Nothing
    it "returns any file name wrapped in Just" $ forAll genVbfName $ \name ->
      osPathToVbfPath name `shouldBe` Just name
    it "returns any file path with its VBF path separators wrapped in Just"
      $ forAll (genVbfPath genVbfName)
      $ \path -> osPathToVbfPath path `shouldBe` Just path
    it
        "returns a file path with its deduplicated VBF path separators wrapped in Just"
      $          osPathToVbfPath "dir1///someDir2/someOther879//file.with.ext"
      `shouldBe` Just "dir1/someDir2/someOther879/file.with.ext"
    it "returns a file path with its leading separator trimmed wrapped in Just"
      $          osPathToVbfPath "/a/file3"
      `shouldBe` Just "a/file3"
    it "returns a file path with its trailing separator trimmed wrapped in Just"
      $          osPathToVbfPath "1/3/d/f/"
      `shouldBe` Just "1/3/d/f"
    it
        "returns the current OS file path converted to VBF separators wrapped in Just"
      $          osPathToVbfPath
                   ("a8" ++ pathSeparator : [] ++ "z9" ++ pathSeparator : [] ++ "d")
      `shouldBe` Just "a8/z9/d"
  describe "vbfPathToOsPath" $ do
    it "returns an empty name if the input is empty"
      $          vbfPathToOsPath ""
      `shouldBe` ""
    it "returns a file name as-is" $ forAll genVbfName $ \name ->
      vbfPathToOsPath name `shouldBe` name
    it
        "returns a VBF path with the separators converted into the current OS separators"
      $ forAll (listOf1 genVbfName)
      $ \names ->
          vbfPathToOsPath (intercalate (pathSeparator : []) names)
            `shouldBe` (intercalate (vbfPathSeparator : []) names)
