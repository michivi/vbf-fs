{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  )
where

import           System.FileSystem.VBF

import           Control.Monad
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as BSL
import           Data.Foldable
import           Data.Function
import           Data.Tree
import qualified Data.Vector                   as Vector
import           Options.Applicative
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO
import           System.ProgressBar

data DumpFormat = ClearTextDump | TsvDump deriving (Eq, Read, Show)

data VBFTool
    = DumpVBFInfo FilePath DumpFormat
    | ExtractEntry FilePath FilePath (Maybe FilePath) ReadingMode (Maybe Integer) (Maybe Integer)
    | TreeVBF FilePath
    | UnpackVBF FilePath (Maybe FilePath)
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

unpackVbf :: Parser VBFTool
unpackVbf =
  UnpackVBF
    <$> argument str (metavar "ARCHIVE" <> help "Archive path")
    <*> option
          (optional str)
          (  long "output"
          <> short 'o'
          <> metavar "OUTPUT"
          <> value Nothing
          <> help
               "Path to the output directory (by default use the name of the archive)"
          )

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
  <> command "unpack"
             (info unpackVbf (progDesc "Unpack the archive to a folder"))
  )

failWithError :: String -> IO ()
failWithError errMsg = do
  hPutStrLn stderr errMsg
  exitWith (ExitFailure 1)

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
  maybe failWithNotFound goExtract $ find matchEntry (vbfcEntries ct)
 where
  failWithNotFound  = failWithError ("File '" ++ entryPath ++ "' not found.")
  requestedPathHash = vbfHashBytes (BS.pack entryPath)
  matchEntry        = (== requestedPathHash) . vbfeArchivePathHash
  entryRange _ Nothing Nothing = EntireFile
  entryRange maxlen off len =
    PartialFile (maybe 0 fromIntegral off) (maybe maxlen fromIntegral len)
  withOutput =
    maybe ((&) stdout) (\fp -> withBinaryFile fp WriteMode) outputPath
  goExtract ei =
    vbfWithfExtractedEntry archivePath
                           ei
                           (entryRange (vbfeSize ei) mboff mblen)
                           mode
      $ \dat -> withOutput $ \hdl -> BSL.hPut hdl dat
run (UnpackVBF archivePath mbOutputDir) = do
  ct <- vbfContent archivePath
  od <- validatedOutputDirOrFail
  goUnpack ct od
 where
  guessOutputDir Nothing  = dropExtensions archivePath
  guessOutputDir (Just p) = p
  validatedOutputDirOrFail = do
    let odir = guessOutputDir mbOutputDir

    pathExists      <- doesPathExist odir
    directoryExists <- doesDirectoryExist odir
    when (pathExists && not directoryExists)
      $ failWithError ("Output path '" ++ odir ++ "' is not a directory.")

    return odir
  goUnpack ct od = do
    let entries = vbfcEntries ct
        cnt     = Vector.length entries
        pbs     = defStyle { stylePrefix = msg "Unpacking" }
    pb <- newProgressBar pbs 10 (Progress 0 cnt ())
    for_ entries $ \entry -> do
      goExtract entry od
      incProgress pb 1
  goExtract entry od = do
    let fp  = BSL.unpack (vbfeArchivePath entry)
        dn  = takeDirectory fp
        odp = od </> dn
        ofp = od </> fp
    createDirectoryIfMissing True odp
    withBinaryFile ofp WriteMode $ \hdl ->
      vbfWithfExtractedEntry archivePath entry EntireFile Decompression
        $ \dat -> BSL.hPut hdl dat

opts :: ParserInfo VBFTool
opts = info (vbfTool <**> helper) idm

main :: IO ()
main = execParser opts >>= run
