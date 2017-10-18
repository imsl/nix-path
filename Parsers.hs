{-# LANGUAGE OverloadedStrings #-}

module Parsers
  ( parseStringOrFail
  , parseTextOrFail
  , parseOrFail
  , nixPaths
  , nixPath
  , nixPathTarget
  )
where

import Types

import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import Network.Parser.Rfc3986 (absoluteUri)
import Network.Types (URI(..), uriScheme)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Data.Text.Encoding

parseStringOrFail :: Parser a -> String -> a
parseStringOrFail p s = parseOrFail p (B.pack s)

parseTextOrFail :: Parser a -> T.Text -> a
parseTextOrFail p t = parseOrFail p (encodeUtf8 t)

parseOrFail :: Parser a -> B.ByteString -> a
parseOrFail p = orFail . parseOnly (p <* endOfInput)
  where orFail (Right a) = a
        orFail (Left _) = error $ "nix path syntax error"

nixPaths :: Parser [NixPath]
nixPaths = nixPath `sepBy1` (char ':')

nixPath :: Parser (NixPath)
nixPath = prefixPath <|> rootPath
  where
    rootPath = fmap RootPath (nixPathTarget id)
    prefixPath = do
      prefix <- many1 $ satisfy $ notInClass ":="
      _ <- char '='
      target <- nixPathTarget id
      return $ PrefixPath prefix target

nixPathTarget :: (FilePath -> FilePath) -> Parser NixPathTarget
nixPathTarget norm = urlTarget <|> filePath norm

-- There is no way to pass a filename containing ':' to nix
-- through NIX_PATH so we simply forbid such paths here
filePath :: (FilePath -> FilePath) -> Parser NixPathTarget
filePath norm = do
  p <- many1 $ satisfy (flip notElem [' ',':'])
  rev' <- option Nothing $ fmap Just $ space *> gitRev
  let uri = URI "file" Nothing p "" ""
  return $ case rev' of
    Nothing -> BasicPath (norm p)
    Just rev -> GitPath uri rev

urlTarget :: Parser NixPathTarget
urlTarget = do
  (bs, uri) <- match absoluteUri
  rev' <- option Nothing $ fmap Just $ space *> gitRev
  return $ case rev' of
    Nothing | elem (uriScheme uri) ["git","ssh"] -> GitPath uri HEAD
    Nothing -> BasicPath (B.unpack bs)
    Just rev -> GitPath uri rev

gitRev :: Parser GitRev
gitRev = hd <|> ref <|> sha <|> branch
  where
    hd = "HEAD" *> return HEAD
    ref = do
      key <- "refs/" *> many1 (notChar '/')
      val <- "/" *> many1 (notChar ':')
      return $ GitRef key val
    sha = fmap GitCommit $ count 40 $ satisfy $ inClass "0123456789abcdef"
    branch = fmap (GitRef "heads") $ many1 $ notChar ':'
