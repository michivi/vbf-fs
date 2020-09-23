module Main
  ( main
  )
where

import           System.FileSystem.VBF

import           Control.Concurrent
import           Control.Exception
import qualified Data.ByteString.Char8         as B
import qualified Data.ByteString.Lazy          as BSL
import           Data.List
import           Data.Tree
import           Foreign.C.Error
import           Options.Applicative
import           System.Environment
import           System.Directory
import           System.FilePath.Posix
import           System.IO
import           System.Posix.Types
import           System.Posix.Files
import           System.Posix.IO
import           System.Fuse

data VBFFSExecutionMode = Background | Foreground deriving (Eq, Show)

data VBFFSVerbosity = Production | Verbose deriving (Eq, Show)

data VBFFSCommand = MountArchive VBFFSVerbosity FilePath FilePath VBFFSExecutionMode

data VerboseOutput
    = ArchiveOpening FilePath
    | EntryReading FilePath FileOffset ByteCount

data VBFFSContext = VBFFSContext FilePath VBFContent VBFTree

data VBFFSHandle = VBFFSHandle VBFEntry (MVar Handle)

mountArchive :: Parser VBFFSCommand
mountArchive =
  MountArchive
    <$> flag Production
             Verbose
             (long "verbose" <> short 'v' <> help "Run in verbose mode")
    <*> argument str (metavar "ARCHIVE" <> help "Path to the VBF archive")
    <*> argument str (metavar "MOUNTPOINT" <> help "Path to the mount point")
    <*> flag Background
             Foreground
             (long "f" <> short 'f' <> help "Run in the foreground")

opts :: ParserInfo VBFFSCommand
opts = info
  (mountArchive <**> helper)
  (fullDesc <> progDesc "Mount a VBF archive as a file system" <> header
    "vbf-fs - FUSE mount tool for VBF archives"
  )

main :: IO ()
main = execParser opts >>= run

fuseOpts :: VBFFSExecutionMode -> [String]
fuseOpts Foreground = ["-f"]
fuseOpts Background = []

logCommand :: VBFFSVerbosity -> VerboseOutput -> IO ()
logCommand Production _                   = return ()
logCommand Verbose    (ArchiveOpening fp) = logMsg ["Opening archive", fp]
logCommand Verbose (EntryReading fp off bc) =
  logMsg ["Reading entry", fp, show off, show bc]

logMsg :: [String] -> IO ()
logMsg parts = hPutStrLn stderr (intercalate "\t" parts) >> hFlush stderr

run :: VBFFSCommand -> IO ()
run (MountArchive verbosity arcPath mntPath execMode) = do
  prog                <- getProgName
  absoluteArchivePath <- makeAbsolute arcPath

  logCommand verbosity (ArchiveOpening absoluteArchivePath)
  ct <- vbfContent arcPath

  let tr   = vbfContentTree ct
      ctxt = VBFFSContext absoluteArchivePath ct tr

  let args = [mntPath] <> fuseOpts execMode
  fuseRun prog args (vbfFSOps verbosity ctxt) defaultExceptionHandler

vbfFSOps :: VBFFSVerbosity -> VBFFSContext -> FuseOperations VBFFSHandle
vbfFSOps verbosity ctxt@(VBFFSContext _ ct tr) = defaultFuseOps
  { fuseGetFileStat        = vbfGetFileStat tr
  , fuseOpen               = vbfOpen ctxt
  , fuseRelease            = vbfRelease
  , fuseRead               = vbfRead verbosity
  , fuseOpenDirectory      = vbfOpenDirectory tr
  , fuseReadDirectory      = vbfReadDirectory tr
  , fuseGetFileSystemStats = vbfGetFileSystemStats ct
  }

vbfFindPath :: VBFTree -> FilePath -> Maybe VBFTree
vbfFindPath tr fp = go tr lvls
 where
  lvls = filter (/= "/") (splitDirectories fp)
  childByName l (Node _ cs) = find ((== l) . vbfNodeName) cs
  go t []       = Just t
  go t (a : as) = childByName a t >>= (`go` as)

dirStat :: FuseContext -> FileStat
dirStat ctx = FileStat
  { statEntryType        = Directory
  , statFileMode         = foldr1
                             unionFileModes
                             [ ownerReadMode
                             , ownerExecuteMode
                             , groupReadMode
                             , groupExecuteMode
                             , otherReadMode
                             , otherExecuteMode
                             ]
  , statLinkCount        = 2
  , statFileOwner        = fuseCtxUserID ctx
  , statFileGroup        = fuseCtxGroupID ctx
  , statSpecialDeviceID  = 0
  , statFileSize         = 4096
  , statBlocks           = 1
  , statAccessTime       = 0
  , statModificationTime = 0
  , statStatusChangeTime = 0
  }

fileStat :: VBFSizeUnit -> FuseContext -> FileStat
fileStat len ctx = FileStat
  { statEntryType        = RegularFile
  , statFileMode         = foldr1 unionFileModes
                                  [ownerReadMode, groupReadMode, otherReadMode]
  , statLinkCount        = 1
  , statFileOwner        = fuseCtxUserID ctx
  , statFileGroup        = fuseCtxGroupID ctx
  , statSpecialDeviceID  = 0
  , statFileSize         = fromIntegral len
  , statBlocks           = 1
  , statAccessTime       = 0
  , statModificationTime = 0
  , statStatusChangeTime = 0
  }

vbfGetFileStat :: VBFTree -> FilePath -> IO (Either Errno FileStat)
vbfGetFileStat tr fp = do
  ctx <- getFuseContext
  case vbfFindPath tr fp of
    Nothing                             -> return (Left eNOENT)
    Just (Node (VBFEntryTag _ len _) _) -> return (Right (fileStat len ctx))
    Just (Node (IntermediateTag _  ) _) -> return (Right (dirStat ctx))

vbfOpenDirectory :: VBFTree -> FilePath -> IO Errno
vbfOpenDirectory tr fp = pure $ maybe eNOENT (const eOK) (vbfFindPath tr fp)

vbfReadDirectory
  :: VBFTree -> FilePath -> IO (Either Errno [(FilePath, FileStat)])
vbfReadDirectory tr fp = case vbfFindPath tr fp of
  Nothing          -> return $ Left eNOENT
  Just (Node _ cs) -> do
    ctxt <- getFuseContext
    let systemDirs = [(".", dirStat ctxt), ("..", dirStat ctxt)]
        toEntry (Node (IntermediateTag n  ) _) = (n, dirStat ctxt)
        toEntry (Node (VBFEntryTag n len _) _) = (n, fileStat len ctxt)
        entries = toEntry <$> cs
    return $ Right (systemDirs <> entries)

vbfOpen
  :: VBFFSContext
  -> FilePath
  -> OpenMode
  -> OpenFileFlags
  -> IO (Either Errno VBFFSHandle)
vbfOpen (VBFFSContext arcPath ct tr) fp mode _ = case vbfFindPath tr fp of
  Nothing                              -> return (Left eNOENT)
  Just (Node (IntermediateTag _   ) _) -> return (Left eISDIR)
  Just (Node (VBFEntryTag _ _ hash) _) -> case mode of
    ReadOnly -> maybe (return $ Left eNOENT) go
      $ find ((== hash) . vbfeArchivePathHash) (vbfcEntries ct)
    _ -> return (Left eACCES)
 where
  go entry = bracketOnError (openBinaryFile arcPath ReadMode) hClose $ \hdl ->
    do
      hdlVar <- newMVar hdl
      return $ Right (VBFFSHandle entry hdlVar)

vbfRelease :: FilePath -> VBFFSHandle -> IO ()
vbfRelease _ (VBFFSHandle _ hdlv) = withMVar hdlv hClose

vbfRead
  :: VBFFSVerbosity
  -> FilePath
  -> VBFFSHandle
  -> ByteCount
  -> FileOffset
  -> IO (Either Errno B.ByteString)
vbfRead verbosity fp (VBFFSHandle entry hdlv) byteCount offset =
  withMVar hdlv $ \hdl -> do
    logCommand verbosity (EntryReading fp offset byteCount)
    Right
      .   BSL.toStrict
      <$> vbfReadEntryContentLazily
            hdl
            entry
            (PartialFile (fromIntegral offset) (fromIntegral byteCount))
            Decompression

vbfGetFileSystemStats
  :: VBFContent -> String -> IO (Either Errno FileSystemStats)
vbfGetFileSystemStats ct _ = return $ Right $ FileSystemStats
  { fsStatBlockSize       = fromIntegral vbfBlockSize
  , fsStatBlockCount      = fromIntegral (vbfcBlockCount ct)
  , fsStatBlocksFree      = 0
  , fsStatBlocksAvailable = 0
  , fsStatFileCount       = fromIntegral $ length (vbfcEntries ct)
  , fsStatFilesFree       = 0
  , fsStatMaxNameLength   = 255
  }
