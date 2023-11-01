{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Text.Gigaparsec ( Parsec, parse, Result (Success, Failure), (<|>), void, many, eof, atomic, notFollowedBy, liftA2 )
import Text.Gigaparsec.Combinator ( sepBy, sepBy1 )
import Text.Gigaparsec.Char ( char, digit, satisfy, string, letter )
import Data.Char ( digitToInt, isSpace, isAlphaNum, isNumber )
import Text.Gigaparsec.Expr.Chain ()
import Data.Kind ( Type )
import Data.String ( IsString(..) )
import Text.Printf (printf)

data Expr = Val Int | Var String deriving (Show)
data Stat = Decl String Expr | Print Expr | Seq Stat Stat deriving (Show)


val :: Applicative f => f Char -> f Expr
val = fmap (Val . digitToInt)

decl :: Applicative f => f String -> f Expr -> f Stat
decl = liftA2 Decl

----- LEXER -----
keyword :: String -> Parsec ()
keyword k = token (string k *> notFollowedBy (satisfy isAlphaNum))

keys :: [String]
keys = ["const"]

whitespace :: Parsec ()
whitespace = void $ many $ satisfy isSpace

fully :: Parsec a -> Parsec a
fully p = whitespace *> p <* eof

lexeme :: Parsec a -> Parsec a
lexeme p = p <* whitespace

token :: Parsec a -> Parsec a
token = lexeme . atomic

instance a~() => IsString (Parsec a) where
    fromString :: String -> Parsec a
    fromString str
        | str `elem` keys = keyword str
        | otherwise = void $ token (string str)


----- INPUTS -----
programA :: String
programA = "\n\n\r\
            \const foo = 3;\
            \print foo\n"

programB :: String
programB = "const foo = 4"

goodArray :: String
goodArray = "[1,2,3,4,5]"

badArray :: String
badArray = "[1,2,,4,5"

----- PARSER -----

parseArray :: Parsec [Char]
parseArray = char '[' *> sepBy digit (char ',') <* char ']'

parseIdent :: Parsec String
parseIdent = token $ many (satisfy isAlphaNum) <* notFollowedBy (satisfy isAlphaNum)

parseExpr :: Parsec Expr
parseExpr = val (satisfy isNumber) <|> Var <$> parseIdent

parseDecl :: Parsec Stat
parseDecl = decl ("const" *> parseIdent) ("=" *> parseExpr)

parsePrint :: Parsec Stat
parsePrint = Print <$> ("print" *> parseExpr)

parseStat :: Parsec Stat
parseStat = parseDecl <|> parsePrint

parseProgram :: Parsec [Stat]
parseProgram = sepBy1 parseStat (char ';')

main :: IO ()
main = do
    case parse (fully parseProgram) program of
        Success success -> mapM_ print success
        Failure fail -> putStrLn fail
    where
        program = programB


