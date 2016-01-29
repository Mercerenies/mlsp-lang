module Lang.PrettyPrinter where

import Lang.Printer
import Data.List
import Data.Function
import Control.Applicative
import Control.Monad

type Choice = []

bigSexp :: Int -> SExpr String
bigSexp = loopMe
    where loopMe 0 = List []
          loopMe n = List [Atom "AAAAAA", Symbol "BBBBBB", loopMe $ n - 1]

prettyShowSexp :: Show a => Int -> SExpr a -> String
prettyShowSexp mx sexp = minimumBy (compare `on` (length . lines)) forms
    where forms = do
            str <- expressAsString 0 sexp
            let column = maximum . map length $ lines str
            guard $ column <= mx
            return str

expressAsString :: Show a => Int -> SExpr a -> Choice String
expressAsString _ (Symbol x) = return x
expressAsString _ (Atom x) = return $ show x
expressAsString _ (List []) = return "()"
expressAsString i (List (x:xs)) = do
  first <- expressAsString i x
  next <- doRestOfList i xs
  return $ "(" ++ first ++ next ++ ")"

doRestOfList :: Show a => Int -> [SExpr a] -> Choice String
doRestOfList _ [] = return ""
doRestOfList i (x:xs) = indent <|> dont
    where indent = do
            first <- expressAsString (i + 4) x
            next <- doRestOfList (i + 4) xs
            return $ "\n" ++ (replicate (i + 4) ' ') ++ first ++ next
          dont = do
            first <- expressAsString i x
            next <- doRestOfList i xs
            return $ " " ++ first ++ next
