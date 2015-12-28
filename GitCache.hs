module GitCache
  ( gitClone
  )
where

import Types

import Control.Monad
import Control.Exception
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import Data.UUID
import Data.UUID.V4
import Network.Types hiding (HEAD)
import System.Directory
import System.Environment
import System.Environment.XDG.BaseDir
import System.Exit
import System.FilePath.Posix
import System.Process
import System.IO

cacheVersion :: String
cacheVersion = "002"

git :: [String] -> IO ()
git args = do
  let cp = (proc "git" args) { std_out = UseHandle stderr }
  (_, _, _, ph) <- createProcess cp
  ex <- waitForProcess ph
  case ex of
    ExitSuccess -> return ()
    ExitFailure _ -> error "git process failed"

ifJust :: Maybe a -> (a -> c) -> c -> c
ifJust = flip $ flip . flip maybe

gitClone :: URI -> GitRev -> IO String
gitClone repoUri rev = do
  [gitDir, wtsDir] <- setupDirs repoUri
  wt' <- validWorktree wtsDir rev
  ifJust wt' return $ do
    setEnv "GIT_DIR" gitDir
    git ["init","--bare"]
    remote <- fmap toString nextRandom
    git ["remote", "add", remote, uriToString repoUri]
    (sha,ref') <- resolveRev rev remote
    let wt = joinPath [wtsDir,sha]
    wtExists <- doesDirectoryExist wt
    unless wtExists $ gitFetch gitDir remote wt sha ref'
    return wt

validWorktree :: FilePath -> GitRev -> IO (Maybe FilePath)
validWorktree wtsDir (GitCommit sha) = do
  let wt = joinPath [wtsDir,sha]
  wtExists <- doesDirectoryExist wt
  return $ if wtExists then Just wt else Nothing
validWorktree _ _ = return Nothing

gitFetch :: FilePath -> String -> FilePath -> String -> Maybe String -> IO ()
gitFetch gitDir remote wt sha ref' = do
  let tmpWt = concat [wt,"-",remote]
      branch = "nixpath/"++sha
  git $ ["fetch","--no-tags",remote] ++ (maybeToList ref')
  git ["branch",branch,sha]
  git ["clone","-b",branch,gitDir,tmpWt]
  handle
    (\(SomeException _) -> removeDirectoryRecursive tmpWt)
    (renameDirectory tmpWt wt)

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

setupDirs :: URI -> IO [FilePath]
setupDirs repoUri = do
  cacheDir <- getUserCacheDir "nix-path"
  forM [combine "git" repo, "wts"] $ \d -> do
    let dir = joinPath [cacheDir,cacheVersion,d]
    createDirectoryIfMissing True dir
    return dir
  where repo = filter isAlphaNum $ concat
                 [ maybe "" uriRegName (uriAuthority (repoUri))
                 , maybe "" uriPort (uriAuthority (repoUri))
                 , uriPath repoUri
                 ]

uriToString :: URI -> String
uriToString uri = concat
  [ uriScheme uri
  , "://"
  , maybe "" mkAuth (uriAuthority uri)
  , uriPath uri
  , maybeString ('?':) (uriQuery uri)
  , maybeString ('#':) (uriFragment uri)
  ]
  where maybeString _ "" = ""
        maybeString f s = f s
        mkAuth auth = concat
          [ maybeString (++"@") (uriUserInfo auth)
          , uriRegName auth
          , maybeString (':':) (uriPort auth)
          ]
