{-# LANGUAGE FlexibleContexts #-}
-- Based on MLSP5.txt

import Lang.Tokens(Lexeme)
import Lang.Lexer
import Lang.Parser
import Lang.Printer
import Text.Parsec(parse)
import Text.Parsec.Error(ParseError)

testParser :: FilePath -> IO (Either ParseError [Decl])
testParser str = readFile str >>= \code -> return $ do
                   lex <- scan str code
                   parse file str lex

testLexer :: FilePath -> IO (Either ParseError [Lexeme])
testLexer str = readFile str >>= \code -> return $ scan str code

-- main :: IO ()
-- main = getContents >>= print . parse tokens "(stdin)"
