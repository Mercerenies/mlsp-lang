module Lang.Loader where

import Lang.Parser

data LoaderNames = LoaderNames
                   deriving (Show, Read, Eq)

loadNames :: FileData -> LoaderNames
loadNames _ = LoaderNames
