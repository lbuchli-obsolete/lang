{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module IR.IRX where

import           Util
import qualified IR.IRY                        as Y
import           Data.Bifunctor                 ( second )

newtype IRX = IRX [Binding] deriving Show
type Const = String
type Lit = String
type Name = String
data Binding = Binding Name [Name] Expr deriving Show
data Expr = Seq Expr Pat Expr
          | Lambda [Pat] Expr
          | Case Expr [(Pat, Expr)]
          | Ap Expr [Expr]
          | CTag Const [Expr]
          | Tag Const
          | Empty
          | Lit Lit
          | Var Name
 deriving Show
data Pat = PatVar Name
         | PatCTag Const [Pat]
         | PatTag Const
         | PatLit Lit
 deriving Show

translate :: IRX -> Result String Y.IRY
translate (IRX bindings) =
  Y.IRY
    .   (\(a, b) -> a ++ b)
    .   second concat
    .   unzip
    <$> translate_bindings 0 bindings
 where
  translate_bindings n ((Binding _name args body) : bs) = do
    (body', bs_append, n') <- translateExpr binding_names n body
    bs'                    <- translate_bindings n' bs
    return
      ( ( Y.Binding _name
                    args
                    (Y.Seq body' (Y.Var $ name n') (Y.Unit (Y.Var $ name n')))
        , bs_append
        )
      : bs'
      )
  translate_bindings _ [] = Success []
  binding_names =
    map (\(Binding _name _ _) -> _name) bindings
      ++ [ "intAdd__"
         , "intGT__"
         , "showHeap"
         , "intPrint_"
         , "strConcat__"
         , "strLength_"
         , "strTake__"
         , "strDrop__"
         , "error"
         ]

type AssignCtr = Int

name :: Int -> String
name x = "#a" ++ show x

translateExpr
  :: [Name]
  -> AssignCtr
  -> Expr
  -> Result String (Y.Expr, [Y.Binding], AssignCtr)
translateExpr bns n (Seq a pat b) = do
  (a'  , bs   , n'  ) <- translateExpr bns n a
  (pat', pre_b, n'' ) <- translatePatLPat bns n' pat
  (b'  , bs'  , n''') <- translateExpr bns n'' b
  return (Y.Seq a' pat' (pre_b b'), bs ++ bs', n''')
translateExpr bns n (Lambda pat expr) = do
  return undefined -- TODO make lambda separate binding
  -- external values not passed as arguments have to be cloned in
translateExpr bns n (Case expr matches) = do
  (pre_case, bs, v, n') <- asVar bns n expr
  (_case, bs', n'')     <- translatePatCase bns n' v matches
  return (pre_case _case, bs ++ bs', n'')
translateExpr bns n (Ap f args) = do
  (f', bs, n')                <- translateExpr bns n f
  (pre_args, bs', args', n'') <- asVars bns n' args
  return
    (case f' of
      (Y.Store (Y.Var fv)) | fv `elem` bns ->
        (pre_args $ Y.Ap fv (map Y.Var args'), bs ++ bs', n'')
      _ -> error (show f') {-
        ( pre_args $ Y.Ap (name n'') (map Y.Var args') -- TODO pass used variables as args, reform args
        , (Y.Binding (name n'') [] f') : bs ++ bs'
        , n'' + 1
        ) -}
    )
translateExpr bns n (CTag c exprs) = to_ctag <$> asVars bns n exprs
 where
  to_ctag (pre_exprs, bs, exprs', n') =
    (pre_exprs (Y.Store $ Y.CTag c (map Y.Var exprs')), bs, n')
translateExpr _ n (Tag c  ) = Success (Y.Store (Y.Tag c), [], n)
translateExpr _ n (Empty  ) = Success (Y.Store Y.Empty, [], n)
translateExpr _ n (Lit lit) = Success (Y.Store (Y.Lit lit), [], n)
translateExpr _ n (Var var) = Success (Y.Store (Y.Var var), [], n)

translatePatLPat
  :: [Name]
  -> AssignCtr
  -> Pat
  -> Result String (Y.LPat, Y.Expr -> Y.Expr, AssignCtr)
translatePatLPat _   n (PatVar var    ) = Success (Y.Var var, id, n)
translatePatLPat bns n (PatCTag c pats) = do
  (pats', pre_b, n') <- translate_pats n pats
  return (Y.CTag c pats', pre_b, n')
 where
  translate_pats _n (x : xs) = do
    (x' , pre_b , n' ) <- translatePatLPat bns _n x
    (xs', pre_b', n'') <- translate_pats n' xs
    return (x' : xs', pre_b' . pre_b, n'')
  translate_pats _n [] = Success ([], id, _n)
translatePatLPat _ n (PatLit lit) = Success (Y.Lit lit, id, n)
translatePatLPat _ n (PatTag c  ) = Success (Y.Tag c, id, n)

-- lets hope i'll never have to touch this again
translatePatCase
  :: [Name]
  -> AssignCtr
  -> String
  -> [(Pat, Expr)]
  -> Result String (Y.Expr, [Y.Binding], AssignCtr)
translatePatCase bns n var branches = do
  (pats, bs, n', bodies) <- modify_branches n branches
  return (Y.Case (Y.Var var) (zip pats bodies), bs, n')
 where
  modify_branch _n (PatVar v      ) body = (Y.CPatVar v, body, _n)
  modify_branch _n (PatCTag c pats) body = (Y.CPatCTag c pats', body', n')
   where
    (pats', body', n') = foldr combine ([], body, _n) pats
    combine pat (_pats, _body, __n) =
      (\(pat', _body', _n') -> (pat' : _pats, _body', _n')) $ wrap __n pat _body
    wrap __n (PatVar v) _body = (v, _body, __n)
    wrap __n pat _body =
      let (pat', _body', __n') = modify_branch __n pat _body
      in  (name __n', Y.Case (Y.Var $ name __n') [(pat', _body')], __n' + 1)
  modify_branch _n (PatTag c  ) body = (Y.CPatTag c, body, _n)
  modify_branch _n (PatLit lit) body = (Y.CPatLit lit, body, _n)

  modify_branch_or _n (PatVar v) body _ = (Y.CPatVar v, body, _n)
  modify_branch_or _n (PatCTag c pats) body rem_expr =
    (Y.CPatVar (name n'), body'', n' + 1)
   where
    (pats', body', n') = foldr combine ([], body, _n) pats
    combine pat (_pats, _body, __n) =
      (\(pat', _body', _n') -> (pat' : _pats, _body', _n')) $ wrap __n pat _body
    wrap __n (PatVar v) _body = (v, _body, __n)
    wrap __n pat _body =
      let (pat', _body', __n') = modify_branch_or __n pat _body rem_expr
      in  (name __n', Y.Case (Y.Var $ name __n') [(pat', _body')], __n' + 1)
    body'' = Y.Case
      (Y.Var $ name n')
      [(Y.CPatCTag c pats', body'), (Y.CPatVar "#irx_pat_other", rem_expr)]
  modify_branch_or _n (PatTag c) body rem_expr =
    (Y.CPatVar (name _n), body', _n + 1)
   where
    body' = Y.Case
      (Y.Var $ name _n)
      [(Y.CPatTag c, body), (Y.CPatVar "#irx_pat_other", rem_expr)]
  modify_branch_or _n (PatLit lit) body rem_expr =
    (Y.CPatVar (name _n), body', _n + 1)
   where
    body' = Y.Case
      (Y.Var $ name _n)
      [(Y.CPatLit lit, body), (Y.CPatVar "#irx_pat_other", rem_expr)]

  modify_branches _n ((pat, body) : []) = do
    (body', bs    , n' ) <- translateExpr bns _n body
    (pat' , body'', n'') <- Success $ modify_branch n' pat body'
    return ([pat'], bs, n'', [body''])
  modify_branches _n ((pat, body) : xs) = do
    (body', bs, n') <- translateExpr bns _n body
    (rem_expr, bs', n'') <- translatePatCase bns n' var xs
    (pat', body'', n''') <- Success $ modify_branch_or n'' pat body' rem_expr
    return ([pat'], bs ++ bs', n''', [body''])
  modify_branches _n [] = Success ([], [], _n, [])

asVar
  :: [Name]
  -> AssignCtr
  -> Expr
  -> Result String (Y.Expr -> Y.Expr, [Y.Binding], Name, AssignCtr)
asVar bns n expr =
  (\(expr', bs, n') -> (Y.Seq expr' (Y.Var $ name n'), bs, name n', n' + 1))
    <$> translateExpr bns n expr

asVars
  :: [Name]
  -> AssignCtr
  -> [Expr]
  -> Result String (Y.Expr -> Y.Expr, [Y.Binding], [Name], AssignCtr)
asVars bns n (expr : exprs) = do
  (pre_expr , bs , _name, n' ) <- asVar bns n expr
  (pre_exprs, bs', names, n'') <- asVars bns n' exprs
  return (pre_expr . pre_exprs, bs ++ bs', _name : names, n'')
asVars _ n [] = Success (id, [], [], n)

{-
example :: IRX
example = IRX
  [ Binding "main" [] (Ap (Var "intPrint") [Ap (Var "countTo") [Lit "0"]])
  , Binding
    "countTo"
    ["n"]
    (Case
      (Ap (Var "intGT") [Var "n", Lit "10"])
      [ (PatTag "CTrue", Var "n")
      , ( PatTag "CFalse"
        , Ap (Var "countTo") [Ap (Var "intAdd") [Var "n", Lit "1"]]
        )
      ]
    )
  ]
-}

example :: IRX
example = IRX
  [ Binding "main" [] (Ap (Var "upto") [Lit "1", Lit "2"])
  , Binding
    "upto"
    ["a", "b"]
    (Case
      (Ap (Var "intGT") [Var "a", Var "b"])
      [ (PatTag "CTrue", Tag "CNil")
      , ( PatTag "CFalse"
        , CTag
          "CCons"
          [ Var "a"
          , Ap (Var "upto") [Ap (Var "intAdd") [Var "a", Lit "1"], Var "b"]
          ]
        )
      ]
    )
  ]
{-
example :: IRX
example = IRX
  [ Binding
    "main"
    []
    (Ap (Var "intPrint") [Ap (Var "sum") [Ap (Var "upto") [Lit "1", Lit "10"]]])
  , Binding
    "upto"
    ["a", "b"]
    (Case
      (Ap (Var "intGT") [Var "a", Var "b"])
      [ (PatTag "CTrue", Tag "CNil")
      , ( PatTag "CFalse"
        , CTag
          "CCons"
          [ Var "a"
          , Ap (Var "upto") [Ap (Var "intAdd") [Var "a", Lit "1"], Var "b"]
          ]
        )
      ]
    )
  , Binding
    "sum"
    ["list"]
    (Case
      (Var "list")
      [ (PatTag "CNil", Seq (Ap (Var "showHeap") []) (PatVar "_") (Tag "CNil"))
      , ( PatCTag "CCons" [PatVar "a", PatVar "b"]
        , Seq (Ap (Var "intPrint") [Var "a"]) (PatVar "__")
          $ Ap (Var "intAdd") [Var "a", Ap (Var "sum") [Var "b"]]
        )
      ]
    )
  ]

-}
