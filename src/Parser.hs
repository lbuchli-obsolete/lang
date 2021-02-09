{-# LANGUAGE LambdaCase #-}
module Parser where

{--
import           Util
import           Control.Applicative
import           Control.Monad                  ( void )

type SParser = Parser String String

parse :: String -> Result (Pos, String) Data
parse s =
  parseSrc (ws *> pData <* ws <* eof) s
    >>= (\(_, _, stack) -> Trace ("Stack: " ++ show stack) $ Success stack)

pData :: SParser Data
pData =
  (build_list <$> (str "(" *> ws *> sepBy ws pData <* ws <* str ")"))
    <|> (const (DInstr Load) <$> str "$")
    <|> (const (DInstr Add) <$> str "+")
    <|> (const (DInstr Define) <$> str "#")
    <|> (DNum . read <$> some (anyOf "0123456789"))
    <|> (DStr <$> (str "\"" *> many (noneOf "\"") <* str "\""))
 where
  build_list (x : y : xs) = DPair (build_list (y : xs), x)
  build_list (x     : []) = x
  build_list []           = error "Empty parenthesis."

sepBy :: SParser a -> SParser b -> SParser [b]
sepBy a b = (:) <$> b <*> many (a *> b)

ws :: SParser ()
ws = void $ many (anyOf "\t \n")

str :: String -> SParser String
str s = Parser $ \i -> if take len i == s
  then Success (drop len i, posOffsetOf s, s)
  else parseError ("Input does not match '" ++ s ++ "'")
  where len = length s

noneOf :: [Char] -> SParser Char
noneOf vs = Parser $ \case
  []                  -> parseError "Empty input"
  (x : _) | elem x vs -> parseError $ "Should not match '" ++ [x] ++ "'"
  (x : xs)            -> Success (xs, posOffsetOf [x], x)

anyOf :: [Char] -> SParser Char
anyOf vs = Parser $ \case
  [] -> parseError "Empty input"
  (x : xs) | elem x vs -> Success (xs, posOffsetOf [x], x)
  (_ : _) -> parseError $ "Input does not match any of '" ++ vs ++ "'"

count :: SParser a -> SParser Int
count p = length <$> many p

eof :: SParser ()
eof = Parser $ \i -> if null i
  then Success ("", Pos 0 0, ())
  else Error (Pos 0 0, "There is still input left: " ++ shorten i)
  where shorten s = take 16 s ++ if length s > 16 then "..." else ""

parseError :: String -> Result (Pos, String) a
parseError s = Error (Pos 0 0, s)
--}
