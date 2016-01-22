-- Based on MLSP5.txt

import Lang.Lexer
import Lang.Parser
import Lang.Printer
import Lang.Identifier -- TODO Remove this; it's just to make the module load for testing
import System.Environment
import System.IO

-- TODO Command line args (System.Console.GetOpt)
main :: IO ()
main = getArgs >>= \args -> case args of
                              [file] -> do
                                        code <- readFile file
                                        let result = do
                                              lex <- scan file code
                                              exp <- parseCode file lex
                                              let exp' = case lispify exp of
                                                           List xs -> xs
                                                           other -> [other]
                                              return $ mapM_ printSexp exp'
                                        case result of
                                          Left expr -> hPutStrLn stderr $ show expr
                                          Right act -> act
                              _ -> putStrLn "Usage: ./main <filename>"
