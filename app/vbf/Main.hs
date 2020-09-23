module Main
  ( main
  )
where

import           System.FileSystem.VBF

import qualified Data.ByteString.Lazy.Char8    as BSL
import           Data.Foldable
import           Data.Function
import           Data.Tree
import           Options.Applicative
import           System.Exit
import           System.IO

data DumpFormat = ClearTextDump | TsvDump deriving (Eq, Read, Show)

data VBFTool
    = DumpVBFInfo FilePath DumpFormat
    | ExtractEntry FilePath FilePath (Maybe FilePath) ReadingMode (Maybe Integer) (Maybe Integer)
    | TreeVBF FilePath
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
    <*> option
          (optional auto)
          (  long "skip"
          <> short 's'
          <> metavar "SKIP_BYTES"
          <> value Nothing
          <> help "Number of bytes to skip"
          )
    <*> option
          (optional auto)
          (  long "length"
          <> short 'l'
          <> metavar "BYTES"
          <> value Nothing
          <> help "Number of bytes to extract"
          )

treeVbf :: Parser VBFTool
treeVbf = TreeVBF <$> argument str (metavar "ARCHIVE")

vbfTool :: Parser VBFTool
vbfTool = subparser
  (  command "dump"
             (info dumpVbfInfo (progDesc "Dump information on a VBF archive"))
  <> command
       "extract"
       (info extractEntry (progDesc "Extract an entry within the VBF archive")
       )
  <> command "tree"
             (info treeVbf (progDesc "Print a tree of the archive content"))
  )

run :: VBFTool -> IO ()
run (DumpVBFInfo path ClearTextDump) = do
  ct <- vbfContent path
  putStrLn $ "Header size: " ++ show (vbfcHeaderLength ct)
  putStrLn $ "Blocks: " ++ show (vbfcBlockCount ct)
  putStrLn $ "Files:"
  for_ (vbfcEntries ct) $ \entry -> do
    putStrLn
      $  "- "
      ++ BSL.unpack (vbfeArchivePath entry)
      ++ " ("
      ++ show (vbfeSize entry)
      ++ "b at "
      ++ show (vbfeOffset entry)
      ++ ")"
run (DumpVBFInfo path TsvDump) = do
  ct <- vbfContent path
  for_ (vbfcEntries ct) $ \entry -> do
    putStrLn
      $  BSL.unpack (vbfeArchivePath entry)
      ++ "\t"
      ++ show (vbfeSize entry)
      ++ "\t"
      ++ show (vbfeCompressedSize entry)
      ++ "\t"
      ++ show (vbfeOffset entry)
run (TreeVBF path) = do
  ct <- vbfContent path
  let tr = vbfContentTree ct
  putStrLn $ drawTree (vbfNodeTagName <$> tr)
run (ExtractEntry archivePath entryPath outputPath mode mboff mblen) = do
  ct <- vbfContent archivePath
  let notFound = do
        hPutStrLn stderr ("File '" ++ entryPath ++ "' not found.")
        exitWith (ExitFailure 1)
      erg _ Nothing Nothing = EntireFile
      erg maxlen off len =
        PartialFile (maybe 0 fromIntegral off) (maybe maxlen fromIntegral len)
      goExtract ei = do
        let withOutput =
              maybe ((&) stdout) (\fp -> withBinaryFile fp WriteMode) outputPath
        vbfWithfExtractedEntry archivePath
                               ei
                               (erg (vbfeSize ei) mboff mblen)
                               mode
          $ \dat -> withOutput $ \hdl -> BSL.hPut hdl dat
  maybe
    notFound
    goExtract
    (find ((== entryPath) . BSL.unpack . vbfeArchivePath) (vbfcEntries ct))

opts :: ParserInfo VBFTool
opts = info (vbfTool <**> helper) idm

main :: IO ()
main = execParser opts >>= run
