{-# LANGUAGE OverloadedStrings #-}

module Dedup
  ( dedupDir
  )
where

import Control.Concurrent.Async (async, wait)
import Control.Monad
import Control.Exception
import qualified Data.ByteString.Lazy as BL
import Data.Digest.XXHash
import Data.DirStream hiding (isDirectory)
import Data.List.Split
import Data.String
import qualified Filesystem.Path as FP
import qualified Filesystem.Path.CurrentOS as FP
import GHC.Conc
import Numeric
import Pipes
import Pipes.Concurrent
import Pipes.Safe (runSafeT, MonadSafe)
import System.Directory
import System.FilePath
import System.Posix.Files
import System.Random

recurseDir :: MonadSafe m => FP.FilePath -> ListT m (FilePath, FileStatus)
recurseDir path = do
  child <- childOf path
  let file = FP.encodeString child
  stat <- liftIO $ getSymbolicLinkStatus file
  if isDirectory stat
    then recurseDir child
    else if isRegularFile stat then return (file,stat) else mzero

dedupDir :: FilePath -> FilePath -> IO ()
dedupDir linkDir dir = do
  rnd <- getStdRandom random
  workerCount <- getNumCapabilities
  (producer, workers) <- withBuffer unbounded
    (\output -> async $ do
      runSafeT $ runEffect $
        every (recurseDir (fromString dir)) >-> toOutput output
      performGC
    )
    (\input ->
      forM [1..workerCount] $ \i -> async $ do
        runEffect (fromInput input >-> worker (i+rnd))
        performGC
    )
  mapM_ wait (producer:workers)
  where
    worker :: Int -> Consumer (FilePath, FileStatus) IO ()
    worker workerId = forever $ do
      (file,stat) <- await
      lift $ dedupFile linkDir file stat workerId

dedupFile :: FilePath -> FilePath -> FileStatus -> Int -> IO ()
dedupFile linkDir file stat workerId = do
  hash <- xxHash <$> BL.readFile file
  let dir = joinPath (linkDir : chunksOf 2 (showHex hash ""))
      link = combine dir (showHex (fileSize stat) "" ++ showHex (fileMode stat) "")
      tmp = combine linkDir (show workerId)
  linkExists <- doesFileExist link
  if linkExists
    then do
      linkStat <- getSymbolicLinkStatus link
      unless (fileID linkStat == fileID stat) (atomicallyLink tmp link file)
    else do
      createDirectoryIfMissing True dir
      atomicallyLink tmp file link

atomicallyLink :: FilePath -> FilePath -> FilePath -> IO ()
atomicallyLink tmp src dst = finally
  (createLink src tmp >> renameFile tmp dst)
  (doesFileExist tmp >>= flip when (removeLink tmp))
