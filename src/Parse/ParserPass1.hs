module Parse.ParserPass1 where

import           Util
import           Parse.ParserLib
import           Data.Bifunctor                 ( first )
import           Control.Applicative
import           Control.Monad                  ( void )
import           Data.List                      ( sort )

type Argument = ()
type Priority = Int
data Associativity = L | R
  deriving (Eq, Show)
data DeclType = Type | Function | Constructor
  deriving (Eq, Show)
type Name = String
type Symbol = String
type Signature = [Either Argument Symbol]
type FPIRW = [FPDecl]
data FPDecl = FPDecl {
  _type :: DeclType,
  _prio :: Priority,
  _assoc :: Associativity,
  _sign :: Signature
} deriving (Eq, Show)

instance Ord FPDecl where
  compare a b = if by_prio == EQ then by_length else by_prio
   where
    by_prio   = compare (_prio b) (_prio a)
    by_length = compare (length $ _sign b) (length $ _sign a)


parse :: String -> Result String FPIRW
parse s = first
  (\(pos, msg) ->
    (show $ row pos + 1) ++ ":" ++ (show $ col pos) ++ " -- " ++ msg
  )
  (   (\(_, _, result) -> sort (result ++ predefined))
  <$> parseSrc (optional _wsNL *> pFile <* optional _wsNL <* eof) s
  )
 where
  predefined =
    [ FPDecl Function 1 L [Right "intAdd", Left (), Left ()]
    , FPDecl Function 1 L [Right "intGT", Left (), Left ()]
    , FPDecl Function 1 L [Right "showHeap"]
    , FPDecl Function 1 L [Right "intPrint", Left ()]
    , FPDecl Function 1 L [Right "error"]
    ]

pFile :: Parser String Error FPIRW
pFile =
  concat
    <$> (many pComment *> sepBy (many pUnusedLine) pDecl <* many pUnusedLine)

pComment :: Parser String Error String
pComment =
  optional _wsNL
    *> str "--"
    *> many (noneOf ['\n'])
    <* str "\n"
    <* optional _wsNL

pDecl :: Parser String Error [FPDecl]
pDecl =
  (:)
    <$> (decl Type <$> (str "<>" *> common))
    <*> (many pUnusedLine *> some pConstr)
    <|> (\x -> [x])
    .   decl Function
    <$> (str "::" *> common)
 where
  decl t ((prio, assoc), sign) = FPDecl t prio assoc sign
  common = (,) <$> pInfo <*> pSign

pUnusedLine :: Parser String Error ()
pUnusedLine =
  void
    $  noMatch (str "::")
    *> noMatch (str "<>")
    *> noMatch (str "<=")
    *> many (noneOf "\n")
    *> str "\n"

pConstr :: Parser String Error FPDecl
pConstr = constr <$> (str "<=" *> pInfo) <*> pSign
  where constr (prio, assoc) sign = FPDecl Constructor prio assoc sign

pInfo :: Parser String Error (Priority, Associativity)
pInfo = str "[" *> info_defined <* str "]" <* ws <|> ws *> pure (1, L)
 where
  info_defined =
    (,) <$> prio <*> assoc <|> (,) 1 <$> assoc <|> (\x -> (x, L)) <$> prio
  prio  = read <$> many (anyOf "012356789")
  assoc = str "L" *> pure L <|> str "R" *> pure R

pSign :: Parser String Error Signature
pSign = many
  (   Right
  <$> (str "'" *> many (noneOf "'#_ \n\t()") <* str "'" <* ws)
  <|> Left
  <$> pArg
  )

pArg :: Parser String Error Argument
pArg =
  void
    $  some (noneOf "\n \t_:()")
    *> str ":"
    *> (parse_brackets <|> parse_stype)
 where
  parse_brackets =
    void $ pKeyword "(" *> sepBy ws (parse_brackets <|> parse_stype) <* pKeyword
      ")"
  parse_stype = void $ some (noneOf "'#_ \n\t()") <* ws

pKeyword :: String -> Parser String Error String
pKeyword s = str s <* _ws
