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
  (\(pos, msg) -> (show $ row pos) ++ ":" ++ (show $ col pos) ++ " -- " ++ msg)
  (   (\(_, _, result) -> result)
  <$> parseSrc (optional _wsNL *> pFile fp <* _ws <* eof) s
  )

pFile :: FPIRW -> Parser String Error IRW
pFile fp = uncurry IRW . partitionEithers <$> many
  (Left <$> pTypeDecl fp <|> Right <$> pFuncDecl fp)

pTypeDecl :: FPIRW -> Parser String Error TypeDef
pTypeDecl _ = pFail "not implemented"

pConstrDecl :: FPIRW -> Parser String Error Constr
pConstrDecl _ = pFail "not implemented"

pFuncDecl :: FPIRW -> Parser String Error FuncDef
pFuncDecl fps = -- TODO parse constraints
  assemble_func
    <$> (pKeyword "::" *> pSignature fps)
    <*> (optional _ws *> pKeyword "\n:=" *> pTyped fps)
  where assemble_func = undefined

pSignature :: FPIRW -> Parser String Error (Name, [(Name, Typed)])
pSignature fps = assemble_sig <$> some (Left <$> pName <|> Right <$> pArg fps)
 where
  assemble_sig ((Left  n  ) : xs) = first (++ "_" ++ n) $ assemble_sig xs
  assemble_sig ((Right arg) : xs) = second (++ [arg]) $ assemble_sig xs
  assemble_sig []                 = ("", [])

pArg :: FPIRW -> Parser String Error (Name, Typed)
pArg fps = (,) <$> some (noneOf ":\n\t ()'\"") <*> (str ":" *> pSTyped fps)

pTyped :: FPIRW -> Parser String Error Typed
pTyped fp = foldr (<|>) (pFail "No typed match") parsers
  where parsers = map (bpFPDecl fp) fp

pSTyped :: FPIRW -> Parser String Error Typed
pSTyped fps =
  (pTyped $ filter (\fp -> length (_sign fp) == 1) fps)
    <|> (pKeyword "(" *> pTyped fps <* pKeyword ")")

-- build a parser for any FPDecl
bpFPDecl :: FPIRW -> FPDecl -> Parser String Error Typed
-- Right associative
bpFPDecl fps (FPDecl t _ R (Left () : sign)) = undefined
-- Left associative
bpFPDecl fps (FPDecl t _ L sign) | last sign == Left () = undefined
-- General case
bpFPDecl fps (FPDecl t _ _ sign) = t_func sign_id <$> sign_parser
 where
  sign_mapped = map (bimap (const $ pSTyped fps) (\s -> str s <* wsNL)) sign
  sign_parser = foldl
    (\prev x -> either (\arg -> (:) <$> arg <*> prev) (\sym -> sym *> prev) x)
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

pKeyword :: String -> Parser String Error String
pKeyword s = str s <* _ws

pInt :: Parser String Error Int
pInt = read <$> some (anyOf "0123456789")

pStr :: Parser String Error String
pStr = str "\"" *> many (noneOf "\"") <* str "\""

pName :: Parser String Error String
pName = str "'" *> many (noneOf "'") <* str "'"
