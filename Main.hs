{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Main where

import System.IO ( hFlush, stdout )
import Control.Applicative ( Alternative(..) )
import Data.Char ( isDigit, isSpace )
import Data.Fixed ( mod' )

data YoctoType = YBool Bool | YString String | YNum Double | YList [YoctoType] | YStatement deriving Eq

instance Show YoctoType where
    show x = case x of
        YBool True -> "true"
        YBool False -> "false"
        YString y -> show y
        YNum y -> if y == fromInteger yi then show yi else show y where yi = round y
        YList y -> show y
        YStatement -> ""

newtype Parser a = P (String -> Maybe (a, String))

parse :: Parser a -> String -> Maybe (a, String)
parse (P p) = p

consume :: Parser Char
consume = P (\x -> case x of
    "" -> Nothing
    (x:xs) -> Just (x, xs))

sat :: (Char -> Bool) -> Parser Char
sat f = do
    x <- consume
    if f x then return x else empty

space :: Parser ()
space = do
    many (sat isSpace)
    return ()

token :: Parser a -> Parser a
token p = do
    space
    p

instance Functor Parser where
    fmap f p = P (\x -> case parse p x of
        Nothing -> Nothing
        Just (res, rem) -> Just (f res, rem))

instance Applicative Parser where
    pure v = P (\x -> Just (v, x))
    p <*> q = P (\x -> case parse p x of
        Nothing -> Nothing
        Just (res, rem) -> parse (res <$> q) rem)

instance Monad Parser where
    p >>= q = P (\x -> case parse p x of
        Nothing -> Nothing
        Just (res, rem) -> parse (q res) rem)

instance Alternative Parser where
    empty = P (const Nothing)
    p <|> q = P (\x -> case parse p x of
        Nothing -> parse q x
        Just (res, rem) -> Just (res, rem))

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string "" = return ""
string (x:xs) = do
    char x
    string xs
    return (x:xs)

digit :: Parser Char
digit = sat isDigit

num :: Parser YoctoType
num = do
    is <- some digit
    fs <- (do
        dot <- char '.'
        y <- some digit
        return (dot : y)) <|> return ""
    return $ YNum $ read $ is ++ fs

str :: Parser YoctoType
str = do
    char '"'
    x <- many (
        do
            string "\\\""
            return '"'
        <|> sat (/= '"'))
    char '"'
    return $ YString x

bool :: Parser YoctoType
bool = YBool <$> (do
        string "true"
        return True
    <|> do
        string "false"
        return False)

list :: Parser YoctoType
list = do
    char '['
    xs <- many (do
        xi <- expr
        token $ char ','
        return xi)
    x <- expr
    char ']'
    return $ YList (xs ++ [x])

-- this seems too verbose, idk if there's a better way

numOp :: (Double -> Double -> Double) -> YoctoType -> YoctoType -> Parser YoctoType
numOp op (YNum x) (YNum y) = return $ YNum $ op x y
numOp op _ _ = empty

boolOp :: (Bool -> Bool -> Bool) -> YoctoType -> YoctoType -> Parser YoctoType
boolOp op (YBool x) (YBool y) = return $ YBool $ op x y
boolOp op _ _ = empty

numCmp :: (Double -> Double -> Bool) -> YoctoType -> YoctoType -> Parser YoctoType
numCmp op (YNum x) (YNum y) = return $ YBool $ op x y
numCmp op _ _ = empty

yConcat :: YoctoType -> YoctoType -> Parser YoctoType
yConcat (YString x) (YString y) = return $ YString (x ++ y)
yConcat (YList x) (YList y) = return $ YList (x ++ y)
yConcat _ _ = empty

unaryMinusOp :: YoctoType -> Parser YoctoType
unaryMinusOp (YNum x) = return $ YNum (-x)
unaryMinusOp _ = empty

unaryNotOp :: YoctoType -> Parser YoctoType
unaryNotOp (YBool x) = return $ YBool (not x)
unaryNotOp _ = empty

binaryOps :: [(String, YoctoType -> YoctoType -> Parser YoctoType)]
binaryOps = [("*", numOp (*)), ("/", numOp (/)), ("%", numOp mod'), ("+", numOp (+)), ("-", numOp (-)),
    ("<", numCmp (<)), ("<=", numCmp (<=)), (">", numCmp (>)), (">", numCmp (>=)),
    ("||", boolOp (||)), ("&&", boolOp (&&)), ("+", yConcat),
    ("==", \x y -> return $ YBool $ x == y), ("!=", \x y -> return $ YBool $ x /= y)]

unaryOp :: Parser YoctoType
unaryOp = do
        char '-'
        x <- expr'
        unaryMinusOp x
    <|> do
        char '!'
        x <- expr'
        unaryNotOp x

expr' :: Parser YoctoType
expr' = token (num <|> str <|> bool <|> list <|> unaryOp
    <|> do
        char '('
        x <- expr
        token $ char ')'
        return x)

infixOp :: (String, YoctoType -> YoctoType -> Parser b) -> YoctoType -> Parser b
infixOp op x = do
    token $ string $ fst op
    y <- expr'
    snd op x y

anyInfixOp :: YoctoType -> Parser YoctoType
anyInfixOp x = do
        x' <- foldl (<|>) empty $ map (`infixOp` x) binaryOps
        anyInfixOp x'
    <|> return x

expr :: Parser YoctoType
expr = do
    x <- expr'
    y <- anyInfixOp x
    space
    return y

repl :: IO ()
repl = do
    putStr ">>> "
    hFlush stdout
    line <- getLine
    putStrLn $ case parse expr line of
        Just (res, "") -> show res
        _ -> "Invalid Syntax"
    repl

main :: IO ()
main = do
    putStrLn "^~=======================~-. YoctoLang Interpreter .-~========================~^"
    repl