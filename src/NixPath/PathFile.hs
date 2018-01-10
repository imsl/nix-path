{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module NixPath.PathFile
  ( readNixPathEnv
  , readPathFile
  )
where

import NixPath.Types
import NixPath.Parsers

import Data.Fix (Fix(..))
import Nix.Parser
import Nix.Eval
import Nix.Expr
import System.Posix.Env
import System.Exit
import qualified Data.Map as M
import qualified Data.Text as T
import qualified System.FilePath as FP

readNixPathEnv :: IO [NixPath]
readNixPathEnv = do
  mnp <- getEnv "NIX_PATH"
  case mnp of
    Nothing -> return []
    Just nixpaths -> return $ parseStringOrFail nixPaths nixpaths

readPathFile :: FilePath -> IO [NixPath]
readPathFile file = parsePathFile file >>= eval
  where
    eval expr = do
      Fix val <- evalExpr expr (nixBuiltins file)
      return $ toPaths val

    toPaths (NVSet m) = map toPath (M.toList m)
    toPaths _ = errorWithoutStackTrace "Invalid path file (attr set expected)"

    toPath (k, Fix (NVStr s)) =
      PrefixPath (T.unpack k) (parseTextOrFail parser s)
      where parser = nixPathTarget (normaliseNixPath file)
    toPath (k, Fix (NVLiteralPath p)) =
      PrefixPath (T.unpack k) (BasicPath p')
      where p' = normaliseNixPath file p
    toPath (k, Fix (NVEnvPath p)) =
      PrefixPath (T.unpack k) (PathRef p)
    toPath (k, Fix nv) =
      errorWithoutStackTrace $
        "Invalid path element " ++ T.unpack k ++
        ". Expected string, got " ++ show nv


parsePathFile :: FilePath -> IO NExpr
parsePathFile file = do
  result <- parseNixFile file
  case result of
    Failure err -> die $ "Failed parsing path file:\n" ++ show err
    Success expr -> pure expr

normaliseNixPath :: FilePath -> FilePath -> FilePath
normaliseNixPath curFile relPath =
  FP.dropTrailingPathSeparator $ FP.normalise $
    FP.combine (FP.takeDirectory curFile) relPath

nixBuiltins :: FilePath -> NValue IO
nixBuiltins curFile = Fix . NVSet . M.fromList $
  [ ("import", Fix nixImport) ]
  where
    nixImport = NVFunction (Param "path") $ \case
      Fix (NVSet m) | Just (Fix (NVLiteralPath file)) <- M.lookup "path" m -> do
        let absPath = normaliseNixPath curFile file
        expr <- parsePathFile absPath
        evalExpr expr (nixBuiltins absPath)
      nv -> die ("Invalid import argument: " ++ show nv)
