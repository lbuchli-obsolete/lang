module Parse.ParserPass2 where

import           Util
import           IR.IRW
import           Parse.ParserPass1              ( FPIRW
                                                , FPDecl(..)
                                                , Associativity(..)
                                                , DeclType(..)
                                                , Signature
                                                , _sign
                                                )
import           Data.Bifunctor                 ( first
                                                , bimap
                                                )
import           Data.Either                    ( partitionEithers
                                                , isLeft
                                                )
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
pTypeDecl fps =
  assemble_type
    <$> (pKeyword "<>" *> pSignature fps)
    <*> many (pKeyword "<|" *> pTyped fps)
    <*> some (pConstrDecl fps)
 where
  assemble_type (name, args) constraints constructors =
    TypeDef name args constraints constructors

pConstrDecl :: FPIRW -> Parser String Error ConstrDef
pConstrDecl fps =
  assemble_constr <$> (pKeyword "<=" *> pSignature fps) <*> many
    (pKeyword "<=|" *> pTyped fps)
 where
  assemble_constr (name, args) constraints = ConstrDef name args constraints

pFuncDecl :: FPIRW -> Parser String Error FuncDef
pFuncDecl fps = -- TODO parse constraints
  assemble_func
    <$> (pKeyword "::" *> pSignature fps)
    <*> (pKeyword "->" *> pTyped fps)
    <*> many (pKeyword ":|" *> pTyped fps)
    <*> (pKeyword ":=" *> pTyped fps)
 where
  assemble_func (name, args) returntype constraints body =
    FuncDef (FuncSig name args returntype constraints) body

pSignature :: FPIRW -> Parser String Error (Name, [(Name, Typed)])
pSignature fps = assemble_sig . reverse <$> some -- I have no idea why the reverse is necessary
  (Left <$> pName <|> Right <$> pArg fps)
 where
  assemble_sig ((Left  n  ) : xs) = first (++ n) $ assemble_sig xs
  assemble_sig ((Right arg) : xs) = bimap (++ "_") (++ [arg]) $ assemble_sig xs
  assemble_sig []                 = ("", [])

pArg :: FPIRW -> Parser String Error (Name, Typed)
pArg fps = (,) <$> some (noneOf ":\n\t ()'\"") <*> (str ":" *> pSTyped fps)

pTyped :: FPIRW -> Parser String Error Typed
pTyped fps =
  foldr (<|>) (pFail "No typed match") parsers
    <|> (TExpr TUnknown <$> pCase fps)
    <|> pSeq fps
    <|> pLambda fps
    <|> pLiteral
    <|> (TType <$> pFnType fps)
    <|> (TExpr TUnknown <$> pVar)
  where parsers = map (bpFPDecl fps) fps

pSTyped :: FPIRW -> Parser String Error Typed
pSTyped fps =
  (pTyped $ filter (\fp -> length (_sign fp) == 1) fps)
    <|> (pKeyword "(" *> pTyped fps <* pKeyword ")")

pLambda :: FPIRW -> Parser String Error Typed
pLambda fps =
  build_lambda
    <$> (pKeyword "\\" *> some (pPat fps))
    <*> (pKeyword "->" *> pTyped fps)
  where build_lambda args body = TExpr TUnknown (Lambda args body)

pSeq :: FPIRW -> Parser String Error Typed
pSeq fps =
  build_seq <$> pPat fps <*> (pKeyword "<-" *> pTyped fps) <*> pTyped fps
  where build_seq pat val after = TExpr TUnknown $ Seq val pat after

pLiteral :: Parser String Error Typed
pLiteral =
  (   const (TType TStr)
    <$> pKeyword "String"
    <|> const (TType TInt)
    <$> pKeyword "Int"
    <|> const (TType TBool)
    <$> pKeyword "Bool"
    <|> const (TType TypeType)
    <$> pKeyword "*"
    <|> const (TType TAny)
    <$> pKeyword "?"
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
    )
    <?> "Cannot parse literal"

-- build a parser for any FPDecl
-- TODO also build for compact form (+ -> _+_)
bpFPDecl :: FPIRW -> FPDecl -> Parser String Error Typed
-- Right associative
bpFPDecl fps (FPDecl t _ R (Left () : sign)) = undefined
-- General case or left associative -- TODO define non-associative and extra rule for left associative
bpFPDecl fps (FPDecl t _ _ sign            ) = t_func sign_id <$> sign_parser
 where
  (sign_id, sign_parser) = bpSignature fps sign pSTyped
  t_func n args = case t of
    Type        -> TType $ RTT n args
    Function    -> TExpr TUnknown $ Ap (TExpr TUnknown $ Var n) args
    Constructor -> TConstr TUnknown $ RTC n args


bpSignature
  :: FPIRW
  -> Signature
  -> (FPIRW -> Parser String Error a)
  -> (Name, Parser String Error [a])
bpSignature fps sign f =
  (sign_id, sign_parser sign_mapped <|> sign_parser sign_compact)
 where
  sign_mapped = map (bimap (const $ f fps) (\s -> pKeyword s)) sign
  sign_parser _sign = foldr
    (\x prev -> either (\arg -> (:) <$> arg <*> prev) (\sym -> sym *> prev) x)
    (const [] <$> pEmpty)
    _sign
  sign_compact   = Right (pKeyword sign_id) : args_only_sign
  args_only_sign = map (const $ Left $ f fps) (filter isLeft sign)
  sign_id        = foldl (\prev x -> prev ++ either (\_ -> "_") id x) "" sign

pCase :: FPIRW -> Parser String Error Expr
pCase fps =
  (   Case
    <$> (pKeyword "case" *> pSTyped fps)
    <*> (pKeyword "of" *> some (pCaseBranch fps))
    )
    <?> "Cannot parse case statement"

pFnType :: FPIRW -> Parser String Error Type
pFnType fps =
  TFn
    <$> (pKeyword "(" *> some (pSTyped fps))
    <*> (pKeyword "->" *> pTyped fps <* pKeyword ")")

pVar :: Parser String Error Expr
pVar = Var <$> pSym <?> "Cannot parse variable"

pCaseBranch :: FPIRW -> Parser String Error (Pat, Typed)
pCaseBranch fps = (,) <$> pPat fps <*> (pKeyword "=>" *> pTyped fps)

pPat :: FPIRW -> Parser String Error Pat
pPat fps =
  pPatLit
    <|> foldr (<|>) (pFail "No tag constructor match")  tags
    <|> foldr (<|>) (pFail "No ctag constructor match") ctags
    <|> PatVar
    <$> pSym
    <|> pKeyword "("
    *>  pPat fps
    <*  pKeyword ")"
 where
  matchables = filter (\d -> _type d == Constructor || _type d == Type) fps
  tags =
    map
        (\c ->
          either (error "Arg(h!)") (\s -> PatTag <$> pKeyword s) $ head $ _sign
            c
        )
      $ filter (\c -> length (_sign c) == 1) matchables
  ctags = map (\c -> ctag_parser $ bpSignature fps (_sign c) pPat)
    $ filter (\c -> length (_sign c) > 1) matchables
  ctag_parser (name, p) = PatCTag name <$> p

pPatLit :: Parser String Error Pat
pPatLit =
  (PatCTag "#C#Int" . (\x -> [PatLit x]) . show <$> pInt)
    <|> (PatCTag "#C#Str" . (\x -> [PatLit x]) <$> pStr)
    <|> (const (PatTag "#C#Bool#True") <$> pKeyword "True")
    <|> (const (PatTag "#C#Bool#False") <$> pKeyword "False")
    <|> (const (PatTag "#Str") <$> pKeyword "String")
    <|> (const (PatTag "#Int") <$> pKeyword "Int")
    <|> (const (PatTag "#Bool") <$> pKeyword "Bool")
    <|> (const (PatTag "#Type") <$> pKeyword "*")
    <|> (const (PatTag "#Any") <$> pKeyword "?")
    <|> (const (PatTag "#Fn") <$> pKeyword "(->)") -- for now, you can only match any function

pInfo :: Parser String Error ()
pInfo = void $ str "[" *> some (anyOf "0123456789LR") *> str "]" *> ws

pSym :: Parser String Error String
pSym = some (noneOf "\t \n#:()") <* _ws

-- TODO find better name
pKeyword :: String -> Parser String Error String
pKeyword s = str s <* _ws

pInt :: Parser String Error Int
pInt = read <$> some (anyOf "-0123456789") <* _ws -- TODO refine negative parsing

pStr :: Parser String Error String
pStr = str "\"" *> many (noneOf "\"") <* pKeyword "\""

pName :: Parser String Error String
pName = str "'" *> many (noneOf "'") <* pKeyword "'"
