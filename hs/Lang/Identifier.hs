module Lang.Identifier where

import Lang.Util

data SymbolicName a = QualifiedName [SymbolicName a] |
                      BasicName a |
                      AtSign a |
                      DollarSign a |
                      PercentSign a
                      deriving (Show, Read, Eq)

translateName :: String -> SymbolicName String
translateName str
    | '.' `elem` str = QualifiedName . map translateName . split '.' $ str
    | otherwise = case str of
                    ('@':xs) -> AtSign xs
                    ('$':xs) -> DollarSign xs
                    ('%':xs) -> PercentSign xs
                    _ -> BasicName str
