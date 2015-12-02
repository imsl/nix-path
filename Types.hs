module Types where

import Network.Types (URI)

data NixPath = RootPath NixPathTarget | PrefixPath String NixPathTarget
  deriving (Show)

data NixPathTarget = BasicPath String | GitPath URI GitRev
  deriving (Show)

data GitRev = GitCommit String | HEAD | GitRef String String
  deriving (Show)
