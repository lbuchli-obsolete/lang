{-# LANGUAGE LambdaCase #-}
module Parse.ParserLib where

import           Util
import           Control.Applicative
import           Control.Monad                  ( void )

type Error = String

sepBy
  :: Parser String Error a -> Parser String Error b -> Parser String Error [b]
sepBy a b = (:) <$> b <*> many (a *> b)

ws :: Parser String Error Int
ws = count (anyOf "\t \n")

wsNL :: Parser String Error ()
wsNL = void $ many (anyOf "\t ") *> optional (str "\n" *> _wsNL)

_wsNL :: Parser String Error ()
_wsNL = void $ many (anyOf "\t ") *> str "\n" *> optional _wsNL

_ws :: Parser String Error Int
_ws = _wsNL *> ws <|> (const (-1) <$> ws)

str :: String -> Parser String Error String
str s = Parser $ \i -> if take len i == s
  then Success (drop len i, posOffsetOf s, s)
  else parseError ("Input does not match '" ++ s ++ "'")
  where len = length s

noneOf :: [Char] -> Parser String Error Char
noneOf vs = Parser $ \case
  []                  -> parseError "Empty input"
  (x : _) | elem x vs -> parseError $ "Should not match '" ++ [x] ++ "'"
  (x : xs)            -> Success (xs, posOffsetOf [x], x)

anyOf :: [Char] -> Parser String Error Char
anyOf vs = Parser $ \case
  [] -> parseError "Empty input"
  (x : xs) | elem x vs -> Success (xs, posOffsetOf [x], x)
  (_ : _) -> parseError $ "Input does not match any of '" ++ vs ++ "'"

count :: Parser String Error a -> Parser String Error Int
count p = length <$> many p

noMatch :: Parser String Error a -> Parser String Error ()
noMatch p = Parser $ \i -> case (toEither $ parseSrc p i) of
  Left  _ -> Success (i, Pos 0 0, ())
  Right _ -> parseError "Should not match"


eof :: Parser String Error ()
eof = Parser $ \i -> if null i
  then Success ("", Pos 0 0, ())
  else Error (Pos 0 0, "There is still input left: " ++ shorten i)
  where shorten s = take 16 s ++ if length s > 16 then "..." else ""

parseError :: String -> Result (Pos, String) a
parseError s = Error (Pos 0 0, s)

pFail :: String -> Parser i Error a
pFail msg = Parser $ \_ -> parseError msg

pEmpty :: Parser i e ()
pEmpty = Parser $ \i -> Success (i, Pos 0 0, ())
