-- Based on MLSP5.txt

import Lang.Printer
import Lang.Identifier -- TODO Remove this; it's just to make the module load for testing
import Lang.Reader
import System.Environment
import System.IO
import Control.Monad.Trans.Except(runExceptT)

-- TODO Command line args (System.Console.GetOpt)
main :: IO ()
main = getArgs >>= \args -> case args of
                              [file] -> do
                                        result <- runExceptT $ parseFile file
                                        case result of
                                          Left expr -> hPutStrLn stderr $ show expr
                                          Right act -> output StdOut act
                              _ -> hPutStrLn stderr "Usage: ./main <filename>"
