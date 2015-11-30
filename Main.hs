module Main where

import Control.Monad
import Data.Fix (Fix(..))
import Data.List
import Data.List.Split
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

data GitBranch = Branch T.Text | HEAD

data NixPathTarget = BasicPath T.Text | GitPath T.Text GitBranch

type NixPath = ((Maybe T.Text), NixPathTarget)

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
  let env' = ("NIX_PATH", T.unpack path):(filter ((/= "NIX_PATH") . fst) env)
  executeFile (head args') True (tail args') (Just env')

mergeNixPaths :: [NixPath] -> [NixPath] -> [NixPath]
mergeNixPaths ps1 ps2 = nubBy f (ps2 ++ ps1)
  where f (Just k1, _) (Just k2, _) = k1 == k2
        f _ _ = False

renderNixPaths :: [NixPath] -> IO T.Text
renderNixPaths = fmap (T.concat . intersperse ":") . mapM renderPath
  where renderPath (Just k, t) = do
          t' <- renderNixPathTarget t
          return $ T.concat [k, "=", t']
        renderPath (Nothing, t) = renderNixPathTarget t

renderNixPathTarget :: NixPathTarget -> IO T.Text
renderNixPathTarget (BasicPath p) = return p
renderNixPathTarget (GitPath url branch) = undefined -- TODO clone git repo

handleOpt :: ProgramOpt -> IO [NixPath]
handleOpt (OptPathFile f) = parsePathFile f
handleOpt (OptNixPath p) = return $ parseNixPaths p
handleOpt OptNixPathEnv = parseNixPathEnv

parseNixPaths :: String -> [NixPath]
parseNixPaths nixpath = map parseNixPath (splitOn ":" nixpath)

parseNixPath :: String -> NixPath
parseNixPath p
   | elem '=' p = let (k,t) = T.breakOn "=" (T.pack p) in (Just k, parseNixPathTarget t)
   | otherwise  = (Nothing, parseNixPathTarget $ T.pack p)

-- TODO Parse git urls
parseNixPathTarget :: T.Text -> NixPathTarget
parseNixPathTarget = BasicPath

parseNixPathEnv :: IO [NixPath]
parseNixPathEnv = do
  mnp <- getEnv "NIX_PATH"
  case mnp of
    Nothing -> return []
    Just nixpaths -> return $ parseNixPaths nixpaths

parsePathFile :: FilePath -> IO [NixPath]
parsePathFile file = parseNixFile file >>= eval
  where
    eval (Failure err) = error $ "Failed parsing path file:\n" ++ show err
    eval (Success expr) = do
      Fix val <- evalExpr expr (Fix (NVSet M.empty))
      return $ toPaths val
    toPaths (NVSet m) = map toPath (M.toList m)
    toPaths _ = error $ "Invalid path file (attr set expected)"
    toPath (k, (Fix (NVStr s))) = (Just k, parseNixPathTarget s)
    toPath (k, (Fix (NVConstant (NPath False p)))) = (Just k, parseNixPathTarget (T.pack p'))
      where p' = FP.normalise $ FP.combine (FP.takeDirectory file) p
    toPath (k, (Fix nv)) = error $ "Invalid path element " ++ (T.unpack k) ++ ". Expected string, got " ++ (show nv)
