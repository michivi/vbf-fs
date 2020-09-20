module Main where

import           Control.Effect.VBFFS
import           Control.Effect.VBFFS.Internal

import           Data.Foldable
import           Options.Applicative

data VBFTool
    = DumpVBFInfo FilePath
    deriving (Eq, Show)

dumpVbfInfo :: Parser VBFTool
dumpVbfInfo = DumpVBFInfo <$> argument str (metavar "ARCHIVE")

vbfTool :: Parser VBFTool
vbfTool = subparser
  (command "dump"
           (info dumpVbfInfo (progDesc "Dump information on a VBF archive"))
  )

run :: VBFTool -> IO ()
run (DumpVBFInfo path) = do
  putStrLn $ "Files:"
  ct <- vbfContent path
  for_ (vbfcEntries ct) $ \entry -> do
    putStrLn $ "- " ++ vbfeArchivePath entry

opts :: ParserInfo VBFTool
opts = info (vbfTool <**> helper) idm

main :: IO ()
main = execParser opts >>= run
