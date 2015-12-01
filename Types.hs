module Types where

data NixPath = RootPath NixPathTarget | PrefixPath String NixPathTarget
  deriving (Show)

data NixPathTarget = BasicPath String | GitPath String (Maybe String)
  deriving (Show)
