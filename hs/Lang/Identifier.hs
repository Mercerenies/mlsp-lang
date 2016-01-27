module Lang.Identifier(PackageName(..), SymbolicName(..),
                       RawName(..), RefName(..), DSName(..),
                       getRawName,
                       getPackageName,
                       translateName, mainPackageName,
                       toPackageName, fromPackageName,
                       toRawName, toRefName, toDSName,
                       getRefIdName, fromRefName) where

import Lang.Util
import Data.List(intercalate)

newtype PackageName = PackageName [String]
    deriving (Show, Read, Eq, Ord)

newtype RawName = RawName String
    deriving (Show, Read, Eq, Ord)

newtype DSName = DSName String
    deriving (Show, Read, Eq, Ord)

data RefName = Raw RawName |
               Qualified PackageName RawName
               deriving (Show, Read, Eq, Ord)

data SymbolicName a = QualifiedName [SymbolicName a] |
                      BasicName a |
                      AtSign a |
                      DollarSign a |
                      PercentSign a
                      deriving (Show, Read, Eq)

getRawName :: RawName -> String
getRawName (RawName str) = str

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

toRawName :: String -> Maybe RawName
toRawName str = case translateName str of
                  BasicName x -> Just $ RawName x
                  _ -> Nothing

toRefName :: String -> Maybe RefName
toRefName str = case translateName str of
                  BasicName x ->
                      Just $ Raw (RawName x)
                  QualifiedName xs -> do
                    pkg <- PackageName <$> mapM getBasicName (init xs)
                    rest <- RawName <$> getBasicName (last xs)
                    return $ Qualified pkg rest
                  _ -> Nothing
    where getBasicName (BasicName x) = Just x
          getBasicName _ = Nothing

fromRefName :: RefName -> String
fromRefName (Raw (RawName x)) = x
fromRefName (Qualified pkg (RawName x)) = fromPackageName pkg ++ "." ++ x

toDSName :: String -> Maybe DSName
toDSName str = case translateName str of
                 DollarSign x -> Just $ DSName x
                 _ -> Nothing

getRefIdName :: RefName -> RawName
getRefIdName (Raw x) = x
getRefIdName (Qualified _ x) = x
