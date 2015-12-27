-- Based on MLSP5.txt

import Lang.Lexer
import Lang.Parser
import Lang.Printer
import System.Environment

-- TODO Command line args (System.Console.GetOpt)
main :: IO ()
main = getArgs >>= \args -> case args of
                              [file] -> do
                                        code <- readFile file
                                        let result = do
                                              lex <- scan file code
                                              exp <- parseCode file lex
                                              return $ mapM_ (output StdOut) exp
                                        case result of
                                          Left expr -> print expr
                                          Right act -> act
                              _ -> putStrLn "Usage: ./main <filename>"
