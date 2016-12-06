{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Types
import GitCache
import qualified Parsers as P

import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Digest.XXHash
import Data.Fix (Fix(..))
import Data.Function (on)
import Data.List
import Nix.Parser
import Nix.Eval
import Numeric
import System.Console.GetOpt
import System.Directory
import System.Environment (getArgs)
import System.FilePath
import System.IO
import System.Posix.Process
import System.Posix.Env
import qualified System.FilePath as FP
import qualified Data.Map as M
import qualified Data.Text as T

data ProgramOpt
     = OptPathFile FilePath
     | OptNixPathEnv
     | OptNixPath String
     | OptSubPath String
     | OptOptimize
     deriving (Eq)

programOptions :: [OptDescr ProgramOpt]
programOptions =
  [ Option "f" ["pathfile"]    (ReqArg OptPathFile "FILE") "read paths from FILE"
  , Option "e" ["environment"] (NoArg OptNixPathEnv)       "read paths from NIX_PATH"
  , Option "I" ["path"]        (ReqArg OptNixPath "PATH")  "add path PATH"
  , Option "s" ["subpath"]     (ReqArg OptSubPath "PATH")  "promote the sub path PATH"
  , Option "O" ["optimize"]    (NoArg OptOptimize)         "optimize the cache"
  ]

main :: IO ()
main = do
  env <- getEnvironment
  args <- getArgs
  let (opts, args', err) = getOpt RequireOrder programOptions args
      opts' = if null opts then [OptPathFile "paths.nix"] else opts
      subpaths = sortBy (flip (compare `on` length)) [s++"." | OptSubPath s <- opts']
  when (not $ null err) $ error $ "Incorrect arguments: " ++ show err
  when (OptOptimize `elem` opts) $ do
    putStrLn "Optimising the nix-path cache..."
    optimizeCache
  when (null args') $ error "No program to run"
  nixpaths <- mapM handleOpt opts'
  let nixpaths' = foldl mergeNixPaths [] nixpaths
      nixpaths'' = mergeNixPaths nixpaths' $ do
                    PrefixPath p t <- nixpaths'
                    Just p' <- map (flip stripPrefix p) subpaths
                    return (PrefixPath p' t)
  nixpaths''' <- fetchNixPaths nixpaths''
  fp <- generateNixPathsFile nixpaths'''
  path <- renderNixPaths $ PrefixPath "nix-paths" (BasicPath fp) : nixpaths'''
  let env' = ("NIX_PATH", path):(filter ((/= "NIX_PATH") . fst) env)
  executeFile (head args') True (tail args') (Just env')

mergeNixPaths :: [NixPath] -> [NixPath] -> [NixPath]
mergeNixPaths ps1 ps2 = nubBy f (ps2 ++ ps1)
  where f (PrefixPath k1 _) (PrefixPath k2 _) = k1 == k2
        f _ _ = False

fetchNixPaths :: [NixPath] -> IO [NixPath]
fetchNixPaths = mapM fetchNixPath
  where
    fetchNixPath (PrefixPath k g@(GitPath _ _)) = fmap (PrefixPath k) (clone g)
    fetchNixPath (RootPath g@(GitPath _ _)) = fmap (RootPath) (clone g)
    fetchNixPath p = return p
    clone (GitPath uri rev) = do
      sha <- gitClone uri rev
      return $ GitPath uri (GitCommit sha)
    clone _ = error "Can't clone non-git path"

renderNixPaths :: [NixPath] -> IO String
renderNixPaths paths = do
  CacheDirs { cdWts = wtsDir } <- getCacheDirs
  let
    renderPath (RootPath t) = renderPathTarget t
    renderPath (PrefixPath p t) = concat [p, "=", renderPathTarget t]
    renderPathTarget (BasicPath p) = p
    renderPathTarget (GitPath _ (GitCommit sha)) = combine wtsDir sha
    renderPathTarget _ = error "Trying to render un-fetched revision"
  return $ concat $ intersperse ":" $ map renderPath paths

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
handleOpt (OptPathFile f) = readPathFile f
handleOpt (OptNixPath p) = return $ P.parseStringOrFail P.nixPaths p
handleOpt OptNixPathEnv = readNixPathEnv
handleOpt _ = return []

readNixPathEnv :: IO [NixPath]
readNixPathEnv = do
  mnp <- getEnv "NIX_PATH"
  case mnp of
    Nothing -> return []
    Just nixpaths -> return $ P.parseStringOrFail P.nixPaths nixpaths

readPathFile :: FilePath -> IO [NixPath]
readPathFile file = parseNixFile file >>= eval
  where
    eval (Failure err) = error $ "Failed parsing path file:\n" ++ show err
    eval (Success expr) = do
      Fix val <- evalExpr expr (Fix (NVSet M.empty))
      return $ toPaths val

    toPaths (NVSet m) = map toPath (M.toList m)
    toPaths _ = error $ "Invalid path file (attr set expected)"

    toPath (k, (Fix (NVStr s))) =
      PrefixPath (T.unpack k) (P.parseTextOrFail P.nixPathTarget s)
    toPath (k, (Fix (NVLiteralPath p))) =
      PrefixPath (T.unpack k) (P.parseStringOrFail P.nixPathTarget p')
      where p' = FP.normalise $ FP.combine (FP.takeDirectory file) p
    toPath (k, (Fix nv)) =
      error $ "Invalid path element " ++ (T.unpack k) ++ ". Expected string, got " ++ (show nv)
