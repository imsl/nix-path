{-# LANGUAGE OverloadedStrings #-}

module GitCache
  ( gitClone
  , optimizeCache
  , getCacheDirs
  )
where

import Types
import Dedup

import Control.Monad
import Control.Exception
import Data.Foldable
import Data.List.Split
import Data.Maybe
import Data.Typeable
import Data.UUID hiding (fromString)
import Data.UUID.V4
import Network.Types hiding (HEAD)
import System.Directory
import System.Environment
import System.Environment.XDG.BaseDir
import System.Exit
import System.FilePath
import System.Posix.Files
import System.Process
import System.IO (stderr)

cacheVersion :: String
cacheVersion = "002"

data GitCacheException
  = GitCmdException
  | GitFileException
  | NixPrefetchException
  deriving (Show, Typeable)

instance Exception GitCacheException

git :: [String] -> IO ()
git args = do
  let cp = (proc "git" args) { std_out = UseHandle stderr }
  (_, _, _, ph) <- createProcess cp
  ex <- waitForProcess ph
  case ex of
    ExitSuccess -> return ()
    ExitFailure _ -> throw GitCmdException

nixPrefetch :: URI -> String -> IO NixPathTarget
nixPrefetch uri sha = do
  CacheDirs { cdWts = wtsDir, cdGit = gitDir } <- getCacheDirs
  setEnv "QUIET" "1"
  setEnv "PRINT_PATH" "1"
  unsetEnv "GIT_DIR"
  let dir = "file://" ++ gitDir
  (ex,out,err) <- readProcessWithExitCode "nix-prefetch-git" [dir,sha] ""
  setEnv "GIT_DIR" gitDir
  case ex of
    ExitSuccess -> do
      let ln = joinPath [wtsDir,sha++".path"]
          storePath = last (lines out)
      createSymbolicLink storePath ln
      return (FetchedGitPath storePath uri (GitCommit sha))
    ExitFailure _ -> do
      putStrLn err
      throw NixPrefetchException

readLink :: FilePath -> IO (Maybe FilePath)
readLink ln = fmap Just (readSymbolicLink ln) `catch` handler
  where
    handler :: IOError -> IO (Maybe FilePath)
    handler _ = return Nothing

findCache :: Bool -> FilePath -> URI -> String -> IO (Maybe NixPathTarget)
findCache True wtsDir uri sha = do
  let ln = joinPath [wtsDir,sha++".path"]
  mp <- readLink ln
  flip (maybe (return Nothing)) mp $ \p -> do
    pathExists <- doesDirectoryExist p
    if pathExists
      then return (Just (FetchedGitPath p uri (GitCommit sha)))
      else do
        removeFile ln
        return Nothing
findCache False wtsDir uri sha = do
  let wt = joinPath [wtsDir,sha]
  wtExists <- doesDirectoryExist wt
  return $ FetchedGitPath wt uri (GitCommit sha) <$ guard wtExists

gitClone :: Bool -> URI -> GitRev -> IO NixPathTarget
gitClone useNixStore uri rev = do
  CacheDirs { cdWts = wtsDir, cdGit = gitDir, cdLinks = linkDir } <- getCacheDirs
  c <- case rev of
         GitCommit sha -> findCache useNixStore wtsDir uri sha
         _ -> return Nothing
  (flip . flip maybe) return c $ do
    remote <- fmap toString nextRandom
    setEnv "GIT_DIR" gitDir
    git ["init","--quiet","--bare"]
    git ["remote", "add", remote, uriToString uri]
    (sha,ref) <- resolveRev rev remote
    c' <- findCache useNixStore wtsDir uri sha
    (flip . flip maybe) return c' $ do
      gitFetch remote sha ref
      if useNixStore
        then nixPrefetch uri sha
        else gitCheckout uri linkDir gitDir remote wtsDir sha

gitFetch :: String -> String -> Maybe String -> IO ()
gitFetch remote sha ref = do
  let branch = "nixpath/"++sha
  git $ ["fetch","--no-tags",remote] ++ (maybeToList ref)
  git ["branch","--force",branch,sha]

gitCheckout :: URI -> FilePath -> FilePath -> String -> FilePath -> String -> IO NixPathTarget
gitCheckout uri linkDir gitDir remote wtsDir sha = flip onException cleanup $ do
  git ["clone","-b",branch,gitDir,tmpWt]
  removeDirectoryRecursive (combine tmpWt ".git")
  dedupDir linkDir tmpWt
  renameDirectory tmpWt wt
  return (FetchedGitPath wt uri (GitCommit sha))
  where
    branch = "nixpath/"++sha
    wt = combine wtsDir sha
    tmpWt = concat [wt,"-",remote]
    cleanup = do
      dirExists <- doesDirectoryExist tmpWt
      when dirExists (removeDirectoryRecursive tmpWt)

resolveRev :: GitRev -> String -> IO (String, Maybe String)
resolveRev HEAD = resolveSha "HEAD"
resolveRev (GitRef key val) = resolveSha $ concat ["refs/",key,"/",val]
resolveRev (GitCommit sha) = resolveRef sha

resolveRef :: String -> String -> IO (String, Maybe String)
resolveRef sha remote = do
  ls <- fmap lines $ readProcess "git" ["ls-remote",remote] ""
  let f (s:_:_) = s == sha
      f _ = False
  return (sha, fmap (!! 1) $ find f $ map (splitOn "\t") ls)

resolveSha :: String -> String -> IO (String, Maybe String)
resolveSha ref remote = do
  ls <- fmap lines $ readProcess "git" ["ls-remote",remote] ""
  let f (_:r:_) = r == ref
      f _ = False
      sha' = fmap head $ find f $ map (splitOn "\t") ls
      sha = maybe (error $ "Could not find SHA for ref "++ref) id sha'
  return (sha, Just ref)

optimizeCache :: IO ()
optimizeCache = do
  CacheDirs { cdWts = wtsDir, cdLinks = linkDir } <- getCacheDirs
  dedupDir linkDir wtsDir

getCacheDirs :: IO CacheDirs
getCacheDirs = do
  cacheDir <- getUserCacheDir "nix-path"
  [git',wts,links,tmp] <- forM ["git", "wts", "links", "tmp"] $ \d -> do
    let dir = joinPath [cacheDir,cacheVersion,d]
    createDirectoryIfMissing True dir
    return dir
  return $ CacheDirs { cdGit = git', cdWts = wts, cdLinks = links, cdTmp = tmp }
