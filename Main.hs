{-# LANGUAGE OverloadedStrings #-}

module Main where

import Types
import GitCache
import qualified Parsers as P

import Control.Monad
import Data.Fix (Fix(..))
import Data.List
import Nix.Parser
import Nix.Eval
import Nix.Types
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Posix.Process
import System.Posix.Env
import qualified System.FilePath as FP
import qualified Data.Map as M
import qualified Data.Text as T

data ProgramOpt = OptPathFile FilePath | OptNixPathEnv | OptNixPath String

programOptions :: [OptDescr ProgramOpt]
programOptions =
  [ Option "f" ["pathfile"]    (ReqArg OptPathFile "FILE") "read paths from FILE"
  , Option "e" ["environment"] (NoArg OptNixPathEnv)       "read paths from NIX_PATH"
  , Option "I" ["path"]        (ReqArg OptNixPath "PATH")  "add path PATH"
  ]

main :: IO ()
main = do
  env <- getEnvironment
  args <- getArgs
  let (opts, args', err) = getOpt RequireOrder programOptions args
      opts' = if null opts then [OptPathFile "paths.nix"] else opts
  when (not $ null err) $ error $ "Incorrect arguments: " ++ show err
  when (null args') $ error "No program to run"
  nixpaths <- mapM handleOpt opts'
  path <- renderNixPaths $ foldl mergeNixPaths [] nixpaths
  let env' = ("NIX_PATH", path):(filter ((/= "NIX_PATH") . fst) env)
  executeFile (head args') True (tail args') (Just env')

mergeNixPaths :: [NixPath] -> [NixPath] -> [NixPath]
mergeNixPaths ps1 ps2 = nubBy f (ps2 ++ ps1)
  where f (PrefixPath k1 _) (PrefixPath k2 _) = k1 == k2
        f _ _ = False

renderNixPaths :: [NixPath] -> IO String
renderNixPaths = fmap (concat . intersperse ":") . mapM renderPath
  where renderPath (PrefixPath p t) = do
          t' <- renderNixPathTarget t
          return $ concat [p, "=", t']
        renderPath (RootPath t) = renderNixPathTarget t

renderNixPathTarget :: NixPathTarget -> IO String
renderNixPathTarget (BasicPath p) = return p
renderNixPathTarget (GitPath url rev) = gitClone url rev

handleOpt :: ProgramOpt -> IO [NixPath]
handleOpt (OptPathFile f) = readPathFile f
handleOpt (OptNixPath p) = return $ P.parseStringOrFail P.nixPaths p
handleOpt OptNixPathEnv = readNixPathEnv

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
    toPath (k, (Fix (NVConstant (NPath False p)))) =
      PrefixPath (T.unpack k) (P.parseStringOrFail P.nixPathTarget p')
      where p' = FP.normalise $ FP.combine (FP.takeDirectory file) p
    toPath (k, (Fix nv)) =
      error $ "Invalid path element " ++ (T.unpack k) ++ ". Expected string, got " ++ (show nv)
