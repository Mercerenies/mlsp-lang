module Lang.Identifier where

import Lang.Util

type PackageName = [String]

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

toPackageName :: String -> Maybe [String]
toPackageName str = case translateName str of
                      QualifiedName xs -> mapM getBasicName xs
                      _ -> Nothing
    where getBasicName (BasicName x) = Just x
          getBasicName _ = Nothing
