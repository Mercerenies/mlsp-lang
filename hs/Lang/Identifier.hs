module Lang.Identifier(PackageName(..), SymbolicName(..),
                       getPackageName,
                       translateName, mainPackageName,
                       toPackageName, fromPackageName) where

import Lang.Util
import Data.List(intercalate)

newtype PackageName = PackageName [String]
    deriving (Show, Read, Eq, Ord)

data SymbolicName a = QualifiedName [SymbolicName a] |
                      BasicName a |
                      AtSign a |
                      DollarSign a |
                      PercentSign a
                      deriving (Show, Read, Eq)

getPackageName :: PackageName -> [String]
getPackageName (PackageName x) = x

translateName :: String -> SymbolicName String
translateName str
    | '.' `elem` str = QualifiedName . map translateName . split '.' $ str
    | otherwise = case str of
                    ('@':xs) -> AtSign xs
                    ('$':xs) -> DollarSign xs
                    ('%':xs) -> PercentSign xs
                    _ -> BasicName str

mainPackageName :: PackageName
mainPackageName = PackageName ["main"]

toPackageName :: String -> Maybe PackageName
toPackageName str = case translateName str of
                      QualifiedName xs -> PackageName <$> mapM getBasicName xs
                      BasicName x -> Just $ PackageName [x]
                      _ -> Nothing
    where getBasicName (BasicName x) = Just x
          getBasicName _ = Nothing

fromPackageName :: PackageName -> String
fromPackageName = intercalate "." . getPackageName
