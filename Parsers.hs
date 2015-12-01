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
import Data.Maybe
import Control.Applicative
import Network.Parser.Rfc3986 (absoluteUri)
import Network.Types (uriScheme)
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

nixPath :: Parser NixPath
nixPath = prefixPath <|> rootPath
  where
    rootPath = fmap RootPath nixPathTarget
    prefixPath = do
      prefix <- many1 $ satisfy $ notInClass ":="
      _ <- char '='
      target <- nixPathTarget
      return $ PrefixPath prefix target

nixPathTarget :: Parser NixPathTarget
nixPathTarget = urlTarget <|> filePath

-- There is no way to pass a filename containing ':' to nix
-- through NIX_PATH so we simply forbid such paths here
filePath :: Parser NixPathTarget
filePath = fmap BasicPath (many1 $ notChar ':')

urlTarget :: Parser NixPathTarget
urlTarget = do
  (s, uri) <- match absoluteUri
  ref <- option Nothing (fmap Just $ space *> (many1 $ notChar ':'))
  return $ case ref of
    Just "HEAD" -> GitPath (B.unpack s) Nothing
    r | isJust r || elem (uriScheme uri) ["git","ssh"] -> GitPath (B.unpack s) r
      | otherwise -> BasicPath (B.unpack s)
