module Parse.ParserPass2 where

import           Util
import           IR.IRW
import           Parse.ParserPass1              ( FPIRW
                                                , FPDecl(..)
                                                , Associativity(..)
                                                , DeclType(..)
                                                , Argument
                                                , Symbol
                                                , _sign
                                                )
import           Data.Bifunctor                 ( first
                                                , second
                                                , bimap
                                                )
import           Data.Either                    ( partitionEithers )
import           Control.Monad                  ( void )
import           Control.Applicative
import           Parse.ParserLib

data Indentation = More
                 | Less
                 | Same

parse :: FPIRW -> String -> Result String IRW
parse fp s = first
  (\(pos, msg) ->
    (show $ row pos + 1) ++ ":" ++ (show $ col pos) ++ " -- " ++ msg
  )
  ((\(_, _, result) -> result) <$> parseSrc (pFile fp) s)

pFile :: FPIRW -> Parser String Error IRW
pFile fp =
  uncurry IRW
    .   partitionEithers
    <$> (optional _wsNL *> manyThen
          (Left <$> pTypeDecl fp <|> Right <$> pFuncDecl fp)
          (optional ws <* eof)
        )

pTypeDecl :: FPIRW -> Parser String Error TypeDef
pTypeDecl _ = pFail "not implemented"

pConstrDecl :: FPIRW -> Parser String Error Constr
pConstrDecl _ = pFail "not implemented"

pFuncDecl :: FPIRW -> Parser String Error FuncDef
pFuncDecl fps = -- TODO parse constraints
  assemble_func
    <$> (pKeyword "::" *> pSignature fps)
    <*> (pKeyword ":=" *> pTyped fps)
  where assemble_func (name, args) body = FuncDef name args [] body

pSignature :: FPIRW -> Parser String Error (Name, [(Name, Typed)])
pSignature fps = sig <$> some (Left <$> pName <|> Right <$> pArg fps)
 where
  sig = first tail . assemble_sig -- remove starting underscore
  assemble_sig ((Left  n  ) : xs) = first (++ "_" ++ n) $ assemble_sig xs
  assemble_sig ((Right arg) : xs) = second (++ [arg]) $ assemble_sig xs
  assemble_sig []                 = ("", [])

pArg :: FPIRW -> Parser String Error (Name, Typed)
pArg fps = (,) <$> some (noneOf ":\n\t ()'\"") <*> (str ":" *> pSTyped fps)

pTyped :: FPIRW -> Parser String Error Typed
pTyped fp =
  (pLiteral <?> "Cannot parse literal")
    <|> foldr (<|>) (pFail "No typed match") parsers
  where parsers = map (bpFPDecl fp) fp

pSTyped :: FPIRW -> Parser String Error Typed
pSTyped fps =
  (pTyped $ filter (\fp -> length (_sign fp) == 1) fps)
    <|> (pKeyword "(" *> pTyped fps <* pKeyword ")")

pLiteral :: Parser String Error Typed
pLiteral =
  const (TType TStr)
    <$> pKeyword "String"
    <|> const (TType TInt)
    <$> pKeyword "Int"
    <|> const (TType TBool)
    <$> pKeyword "Bool"
    <|> const (TConstr TBool (CBool True))
    <$> pKeyword "True"
    <|> const (TConstr TBool (CBool False))
    <$> pKeyword "False"
    <|> TConstr TInt
    .   CInt
    <$> pInt
    <|> TConstr TStr
    .   CStr
    <$> pStr

-- build a parser for any FPDecl
-- todo also build for compact form (+ -> _+_)
bpFPDecl :: FPIRW -> FPDecl -> Parser String Error Typed
-- Right associative
bpFPDecl fps (FPDecl t _ R (Left () : sign)) = undefined
-- General case or left associative
bpFPDecl fps (FPDecl t _ _ sign            ) = t_func sign_id <$> sign_parser
 where
  sign_mapped = map (bimap (const $ pSTyped fps) (\s -> pKeyword s)) sign
  sign_parser = foldr
    (\x prev -> either (\arg -> (:) <$> arg <*> prev) (\sym -> sym *> prev) x)
    (const [] <$> pEmpty)
    sign_mapped
  sign_id = foldr (\x prev -> prev ++ either (\_ -> "_") id x) "" sign
  t_func n args = case t of
    Type        -> TType $ RTT n args
    Function    -> TExpr TUnknown $ Ap (TExpr TUnknown $ Var n) args
    Constructor -> TConstr TUnknown $ RTC n args

pCase :: FPIRW -> Parser String Error Expr
pCase fps =
  Case
    <$> (pKeyword "case" *> pSTyped fps)
    <*> (pKeyword "of" *> some (pCaseBranch fps))

pCaseBranch :: FPIRW -> Parser String Error (Pat, Typed)
pCaseBranch fps = (,) <$> pPat <*> (pKeyword "=>" *> pTyped fps)

pPat :: Parser String Error Pat
pPat =
  PatVar
    <$> pSym
    <|> PatCTag
    <$> (pKeyword "(" *> pSym)
    <*> (some pPat <* pKeyword ")")

pInfo :: Parser String Error ()
pInfo = void $ str "[" *> some (anyOf "0123456789LR") *> str "]" *> ws

pSym :: Parser String Error String
pSym = some (noneOf "\t \n#:()") <* _ws

-- TODO find better name
pKeyword :: String -> Parser String Error String
pKeyword s = str s <* _ws

pInt :: Parser String Error Int
pInt = read <$> some (anyOf "0123456789") <* _ws

pStr :: Parser String Error String
pStr = str "\"" *> many (noneOf "\"") <* pKeyword "\""

pName :: Parser String Error String
pName = str "'" *> many (noneOf "'") <* pKeyword "'"
