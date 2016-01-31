-- Based on MLSP5.txt

-- ///// source-of-instances (Move instances and generic implementations to
--                            a new slot in the symbol interface)

import Lang.Validator -- DEBUG CODE
import Lang.Printer
import Lang.Loader
import Lang.Reader
import Lang.Options
import System.Environment
import System.IO
import System.Exit
import System.Console.GetOpt(usageInfo)
import Control.Monad.Trans.Except(runExceptT)

main' :: IO ()
main' = do
  args <- getArgs
  case args of
    [file] -> do
              result <- runExceptT $ parseFile file
              case result of
                Left expr -> hPutStrLn stderr $ show expr
                Right act -> outputLisp StdOut act
    _ -> hPutStrLn stderr "Usage: ./main <filename>"

main :: IO ()
main = do
  opts <- doOptions <$> getArgs
  case opts of
    Left errs -> do
              die $ concat errs ++ "\n" ++ errorHeader
    Right (Help, _) -> do
              putStrLn $ usageInfo header options
    Right (DoCompile Nothing _, _) -> do
              die $ "No input files\n" ++ errorHeader
    Right (DoParse Nothing _, _) -> do
              die $ "No input files\n" ++ errorHeader
    Right (DoCompile (Just inp) out, _) -> do
              result <- runExceptT $ parseFile inp
              result' <- case result of
                           Left err -> return $ Left err
                           Right act -> loadMain inp act
              case result' of
                Left err -> die $ show err
                Right (act, _, warns) -> do
                             mapM_ (hPrint stderr) warns
                             outputLisp out act
    Right (DoParse (Just inp) out, _) -> do
              result <- runExceptT $ parseFile inp
              case result of
                Left err -> die $ show err
                Right act -> outputLisp out act
 where header = "Usage: ./main [OPTION...] files...\n"
       errorHeader = header ++ "Try --help for more information\n"
