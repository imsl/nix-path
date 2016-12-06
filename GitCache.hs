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
import System.Process
import System.IO (stderr)

cacheVersion :: String
cacheVersion = "002"

data GitCacheException
  = GitCmdException
  | GitFileException
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

gitClone :: URI -> GitRev -> IO String

gitClone repoUri rev@(GitCommit sha) = do
  CacheDirs { cdWts = wtsDir } <- getCacheDirs
  let wt = joinPath [wtsDir,sha]
  wtExists <- doesDirectoryExist wt
  if wtExists then return sha else gitCloneNew repoUri rev

gitClone repoUri rev = gitCloneNew repoUri rev

gitCloneNew :: URI -> GitRev -> IO String
gitCloneNew repoUri  rev = do
  CacheDirs { cdWts = wtsDir, cdGit = gitDir, cdLinks = linkDir } <- getCacheDirs
  setEnv "GIT_DIR" gitDir
  git ["init","--quiet","--bare"]
  remote <- fmap toString nextRandom
  git ["remote", "add", remote, uriToString repoUri]
  (sha,ref') <- resolveRev rev remote
  let wt = joinPath [wtsDir,sha]
  wtExists <- doesDirectoryExist wt
  unless wtExists $ gitFetch linkDir gitDir remote wt sha ref'
  return sha

gitFetch :: FilePath -> FilePath -> String -> FilePath -> String -> Maybe String -> IO ()
gitFetch linkDir gitDir remote wt sha ref' = do
  let tmpWt = concat [wt,"-",remote]
      branch = "nixpath/"++sha
  flip onException (cleanupDir tmpWt) $ do
    git $ ["fetch","--no-tags",remote] ++ (maybeToList ref')
    git ["branch","--force",branch,sha]
    git ["clone","-b",branch,gitDir,tmpWt]
    removeDirectoryRecursive (combine tmpWt ".git")
    dedupDir linkDir tmpWt
    renameDirectory tmpWt wt
  where
    cleanupDir dir = do
      dirExists <- doesDirectoryExist dir
      when dirExists (removeDirectoryRecursive dir)

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
