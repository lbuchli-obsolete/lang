module IR.IRW where

import qualified IR.IRX                        as X
import           Util

data IRW = IRW [TypeDef] [FuncDef]
   deriving Show

type Const = String
type Lit = String
type Name = String

data FuncDef = FuncDef {
  _bname :: Name,
  _bargs :: [(Name, Typed)],
  _breturntype :: Typed,
  _bconstraints :: [Typed],
  _bbody :: Typed
} deriving Show

data TypeDef = TypeDef {
  _tname :: Name,
  _targs :: [(Name, Typed)],
  _tconstraints :: [Typed],
  _tbody :: [ConstrDef]
} deriving Show

data ConstrDef = ConstrDef {
  _cname :: Name,
  _cargs :: [(Name, Typed)],
  _cconstraints :: [Typed]
} deriving Show

data Type = TUnknown
          | TStr
          | TInt
          | TBool
          | TypeType
          | TAny
          | TFn [Typed] Typed
          | RTT Name [Typed] -- This is confusing as it suggests that the type gets
          -- checked at runtime even though it doesn't, in the next iteration, this
          -- should be named differently
 deriving Show

data Constr = CInt Int
            | CStr String
            | CBool Bool
            | RTC Name [Typed]
 deriving Show

data Expr = Seq Typed Pat Typed
          | Lambda [Pat] Typed
          | Case Typed [(Pat, Typed)]
          | Ap Typed [Typed]
          | Var Name
 deriving Show

data Pat = PatVar Name
         | PatCTag Const [Pat]
         | PatTag Const
         | PatLit Lit
 deriving Show

data Typed = TExpr Type Expr
           | TType Type
           | TConstr Type Constr
 deriving Show

translate :: IRW -> Result String X.IRX
translate irw = Trace ("IRW:: " ++ show irw) $ typeCheck irw >>= translate_irw
 where
  translate_irw (IRW _ fds) = X.IRX <$> mapM translate_funcdef fds
  translate_funcdef (FuncDef name args _ _ body) =
    (\body' -> X.Binding name (map fst args) body') <$> translateTyped body

translateTyped :: Typed -> Result String X.Expr
translateTyped (TExpr _ expr    ) = translateExpr expr
translateTyped (TType t         ) = translateType t
translateTyped (TConstr _ constr) = translateConstr constr

translateType :: Type -> Result String X.Expr
translateType TUnknown     = Error "Unknown type used as value"
translateType TStr         = Success (X.Tag "#Str")
translateType TInt         = Success (X.Tag "#Int")
translateType TBool        = Success (X.Tag "#Bool")
translateType TypeType     = Success (X.Tag "#Type")
translateType TAny         = Success (X.Tag "#Any") -- TODO should this be a variable in order to match everything?
translateType (TFn _ _   ) = Success (X.Tag "#Fn") -- TODO match dependent on types
translateType (RTT n args) = X.CTag n <$> mapM translateTyped args

translateExpr :: Expr -> Result String X.Expr
translateExpr (Seq a pat b) = do
  a'   <- translateTyped a
  pat' <- translatePat pat
  b'   <- translateTyped b
  return (X.Seq a' pat' b')
translateExpr (Lambda pats b) = do
  pats' <- mapM translatePat pats
  b'    <- translateTyped b
  return (X.Lambda pats' b')
translateExpr (Case v matches) = do
  v'         <- translateTyped v
  match_pats <- mapM translatePat (map fst matches)
  match_bs   <- mapM translateTyped (map snd matches)
  return (X.Case v' (zip match_pats match_bs))
translateExpr (Ap f args) = do
  f'    <- translateTyped f
  args' <- mapM translateTyped args
  return (X.Ap f' args')
translateExpr (Var n) = Success (X.Var n)

translateConstr :: Constr -> Result String X.Expr
translateConstr (CInt x) = Success $ X.CTag "#C#Int" [X.Lit $ show x]
translateConstr (CStr s) = Success $ X.CTag "#C#Str" [X.Lit $ s]
translateConstr (CBool b) =
  Success $ X.Tag ("#C#Bool#" ++ if b then "True" else "False")
translateConstr (RTC name []) = Success $ X.Tag name
translateConstr (RTC name ts) = X.CTag name <$> mapM translateTyped ts

translatePat :: Pat -> Result String X.Pat
translatePat (PatVar n      ) = Success $ X.PatVar n
translatePat (PatCTag c pats) = X.PatCTag c <$> mapM translatePat pats
translatePat (PatTag c      ) = Success $ X.PatTag c
translatePat (PatLit lit    ) = Success $ X.PatLit lit

typeCheck :: IRW -> Result String IRW
typeCheck = Success
