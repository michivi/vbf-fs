module Main where

import           Control.Effect.VBFFS
import           Control.Effect.VBFFS.Internal

import qualified Data.ByteString.Lazy.Char8    as BSL
import           Data.Foldable
import           Data.Function
import           Options.Applicative
import           System.Exit
import           System.IO

data DumpFormat = ClearTextDump | TsvDump deriving (Eq, Read, Show)

data VBFTool
    = DumpVBFInfo FilePath DumpFormat
    | ExtractEntry FilePath FilePath (Maybe FilePath) ExtractionMode
    deriving (Eq, Show)

dumpVbfInfo :: Parser VBFTool
dumpVbfInfo = DumpVBFInfo <$> argument str (metavar "ARCHIVE") <*> option
  auto
  (  long "format"
  <> short 'f'
  <> metavar "DUMP_FORMAT"
  <> help "Output dump format"
  <> value ClearTextDump
  )

extractEntry :: Parser VBFTool
extractEntry =
  ExtractEntry
    <$> argument str (metavar "ARCHIVE" <> help "Archive path")
    <*> argument str (metavar "ENTRY" <> help "Path within the archive")
    <*> option
          (optional str)
          (  long "output"
          <> short 'o'
          <> metavar "OUTPUT"
          <> value Nothing
          <> help "Path to the output file (none for stdout)"
          )
    <*> flag Decompression
             RawExtraction
             (long "raw" <> short 'r' <> help "Raw extract")

vbfTool :: Parser VBFTool
vbfTool = subparser
  (  command "dump"
             (info dumpVbfInfo (progDesc "Dump information on a VBF archive"))
  <> command
       "extract"
       (info extractEntry (progDesc "Extract an entry within the VBF archive")
       )
  )

run :: VBFTool -> IO ()
run (DumpVBFInfo path ClearTextDump) = do
  putStrLn $ "Files:"
  ct <- vbfContent path
  for_ (vbfcEntries ct) $ \entry -> do
    putStrLn
      $  "- "
      ++ vbfeArchivePath entry
      ++ " ("
      ++ show (vbfeSize entry)
      ++ "b at "
      ++ show (vbfeOffset entry)
      ++ ")"
run (DumpVBFInfo path TsvDump) = do
  ct <- vbfContent path
  for_ (vbfcEntries ct) $ \entry -> do
    putStrLn
      $  vbfeArchivePath entry
      ++ "\t"
      ++ show (vbfeSize entry)
      ++ "\t"
      ++ show (vbfUncompressedEntrySize entry)
      ++ "\t"
      ++ show (vbfeOffset entry)
run (ExtractEntry archivePath entryPath outputPath mode) = do
  ct <- vbfContent archivePath
  let notFound = do
        hPutStrLn stderr ("File '" ++ entryPath ++ "' not found.")
        exitWith (ExitFailure 1)
      goExtract ei = do
        let withOutput =
              maybe ((&) stdout) (\fp -> withBinaryFile fp WriteMode) outputPath
        vbfExtractEntry archivePath ei mode
          $ \dat -> withOutput $ \hdl -> BSL.hPut hdl dat
  maybe notFound
        goExtract
        (find ((== entryPath) . vbfeArchivePath) (vbfcEntries ct))

opts :: ParserInfo VBFTool
opts = info (vbfTool <**> helper) idm

main :: IO ()
main = execParser opts >>= run
