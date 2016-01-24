module Lang.Options(In(..), Opt(..), options, doOptions) where

import System.Console.GetOpt
import Lang.Printer(Output(..), maybeToOutput)

data In = StdIn |
          FileIn FilePath
          deriving (Show, Read, Eq, Ord)

data Opt = DoCompile (Maybe FilePath) Output |
           DoParse (Maybe FilePath) Output |
           Help
           deriving (Show, Read, Eq)

inputFile :: String -> Opt -> Opt
inputFile file (DoCompile _ output) = DoCompile (Just file) output
inputFile file (DoParse _ output) = DoParse (Just file) output
inputFile _ opt = opt

outputFile :: Maybe String -> Opt -> Opt
outputFile file (DoCompile input _) = DoCompile input $ maybeToOutput file
outputFile file (DoParse input _) = DoParse input $ maybeToOutput file
outputFile _ opt = opt

helpOption :: Opt -> Opt
helpOption = const Help

parseOnly :: Opt -> Opt
parseOnly (DoCompile a b) = DoParse a b
parseOnly opt = opt

options :: [OptDescr (Opt -> Opt)]
options = [
  Option ['c'] ["compile"] (ReqArg inputFile "FILE") "input FILE",
  Option ['o'] ["output"] (OptArg outputFile "FILE") "output FILE",
  Option ['p'] ["parse"] (NoArg parseOnly) "only perform parse step",
  Option ['?', 'h'] ["help"] (NoArg helpOption) "show help information"
 ]

doOptions :: [String] -> Either [String] (Opt, [String])
doOptions args = case getOpt RequireOrder options args of
                   (o, n, []) -> Right (foldr (.) id o $ DoCompile Nothing StdOut, n)
                   (_, _, xs) -> Left xs
