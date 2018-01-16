{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import GitCache
import NixPath.PathFile
import NixPath.Types
import qualified NixPath.Parsers as P

import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Digest.XXHash
import Data.Function (on)
import Data.List
import Numeric
import System.Console.GetOpt
import System.Exit
import System.Directory
import System.Environment (getArgs)
import System.FilePath
import System.IO
import System.Posix.Process
import System.Posix.Env
import qualified System.FilePath as FP

data ProgramOpt
     = OptPathFile FilePath
     | OptNixPathEnv
     | OptNixPath String
     | OptSubPath String
     | OptOptimize
     | OptUseNixStore
     deriving (Eq)

programOptions :: [OptDescr ProgramOpt]
programOptions =
  [ Option "f" ["pathfile"]      (ReqArg OptPathFile "FILE") "read paths from FILE"
  , Option "e" ["environment"]   (NoArg OptNixPathEnv)       "read paths from NIX_PATH"
  , Option "I" ["path"]          (ReqArg OptNixPath "PATH")  "add path PATH"
  , Option "s" ["subpath"]       (ReqArg OptSubPath "PATH")  "promote the sub path PATH"
  , Option "O" ["optimize"]      (NoArg OptOptimize)         "optimize the cache"
  , Option "n" ["use-nix-store"] (NoArg OptUseNixStore)     "add paths to the nix store"
  ]

main :: IO ()
main = do
  env <- getEnvironment
  args <- getArgs
  useNixStoreEnv <- getEnvDefault "NIX_PATH_USE_NIX_STORE" "0"
  let (opts, args', err) = getOpt RequireOrder programOptions args
      opts' = if null opts then [OptPathFile "paths.nix"] else opts
      subpaths = sortBy (flip compare `on` length) [s++"." | OptSubPath s <- opts']
      useNixStore = OptUseNixStore `elem` opts' || useNixStoreEnv /= "0"
  unless (null err) $ die $ "Incorrect arguments: " ++ show err
  when (OptOptimize `elem` opts) $ do
    putStrLn "Optimising the nix-path cache..."
    optimizeCache
  when (null args') $ die "No program to run"
  nixpaths <- mapM handleOpt opts'
  let nixpaths' = foldl mergeNixPaths [] nixpaths
      nixpaths'' = mergeNixPaths nixpaths' $ do
                    PrefixPath p t <- nixpaths'
                    Just p' <- map (`stripPrefix` p) subpaths
                    return (PrefixPath p' t)
  nixpaths''' <- fetchNixPaths useNixStore nixpaths''
  fp <- generateNixPathsFile nixpaths'''
  CacheDirs { cdGit = gitDir } <- getCacheDirs
  let path = renderNixPaths $ PrefixPath "nix-paths" (BasicPath fp) : nixpaths'''
      env' = ("NIX_PATH_GIT_CACHE", gitDir) : ("NIX_PATH", path) : filter ((/= "NIX_PATH") . fst) env
  executeFile (head args') True (tail args') (Just env')

mergeNixPaths :: [NixPath] -> [NixPath] -> [NixPath]
mergeNixPaths ps1 ps2 = nubBy f (ps2 ++ ps1)
  where f (PrefixPath k1 _) (PrefixPath k2 _) = k1 == k2
        f _ _ = False

fetchNixPaths :: Bool -> [NixPath] -> IO [NixPath]
fetchNixPaths useNixStore = mapM fetchNixPath
  where
    fetchNixPath (PrefixPath k (GitPath uri rev)) =
      fmap (PrefixPath k) (gitClone useNixStore uri rev)
    fetchNixPath (RootPath (GitPath uri rev)) =
      fmap RootPath (gitClone useNixStore uri rev)
    fetchNixPath p = return p

renderNixPaths :: [NixPath] -> String
renderNixPaths paths = intercalate ":" $ map renderPath paths
  where
    renderPath (RootPath t) = renderPathTarget t
    renderPath (PrefixPath p t) = concat [p, "=", renderPathTarget t]
    renderPathTarget (BasicPath p) = p
    renderPathTarget (FetchedGitPath p _ _) = p
    renderPathTarget (PathRef p) = resolvePathRef paths p
    renderPathTarget _ = errorWithoutStackTrace "Trying to render un-fetched revision"

resolvePathRef :: [NixPath] -> FilePath -> String
resolvePathRef paths p =
  case targets of
    (BasicPath p'):[] -> FP.joinPath (p':ps)
    (FetchedGitPath p' _ _):[] -> FP.joinPath (p':ps)
    (PathRef _):[] -> errorWithoutStackTrace $
      "The path reference " ++ p ++ " can't refer to another path reference"
    _:_:[] -> errorWithoutStackTrace $
      "Multiple paths matches the reference " ++ p
    _ -> errorWithoutStackTrace $ "No path matches the reference " ++ p
  where
    (root:ps) = splitDirectories p
    targets = [ t | PrefixPath p' t <- paths, p' == root ]

generateNixPathsFile :: [NixPath] -> IO FilePath
generateNixPathsFile paths = do
  CacheDirs { cdTmp = tmpDir } <- getCacheDirs
  let contents = nixPathsToNixExpr paths
      fp = combine tmpDir (showHex (xxHash (BL.pack contents)) "") ++ ".nix"
  fileExist <- doesFileExist fp
  if fileExist
    then return fp
    else do
      (tmpFile,handle) <- openTempFile tmpDir "nix-paths"
      hPutStr handle contents
      hClose handle
      renameFile tmpFile fp
      return fp

handleOpt :: ProgramOpt -> IO [NixPath]
handleOpt (OptPathFile f) = makeAbsolute f >>= readPathFile
handleOpt (OptNixPath p) = return $ P.parseStringOrFail P.nixPaths p
handleOpt OptNixPathEnv = readNixPathEnv
handleOpt _ = return []
