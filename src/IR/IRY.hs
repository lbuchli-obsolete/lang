{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module IR.IRY where

import           Util
import qualified IR.IRZ                        as Z
import           Data.Bifunctor                 ( second )
import           Data.Maybe                     ( mapMaybe )

newtype IRY = IRY [Binding] deriving Show
type Const = String
type Lit = String
type Name = String
data Binding = Binding Name [Name] Expr deriving Show
data Expr = Seq Expr LPat Expr
          | Case Val [(CPat, Expr)]
          | Ap Name [Val]
          | Store Val
          | Fetch Name
          | Update Name Val
          | Unit Val
  deriving Show
data Val = CTag Const [Val]
         | VTag Name [Val]
         | Tag Const
         | Empty
         | Lit Lit
         | Var Name
  deriving Show
type LPat = Val
data CPat = CPatCTag Const [Name]
          | CPatTag Const
          | CPatLit Lit
          | CPatVar Name
  deriving Show

translate :: IRY -> Result String Z.IRZ
translate (IRY bindings) =
  (\(x, _, _) -> Z.IRZ . addApply . addEval $ x)
    <$> mapValuesResult (\b n _ -> translateBinding b n []) (bindings, 0, [])

type VarMap = [(String, Int)]
type SSACtr = Int

mapValue :: (a -> b) -> (a, n, m) -> (b, n, m)
mapValue f (a, n, m) = (f a, n, m)

mapValues :: (a -> n -> m -> (b, n, m)) -> ([a], n, m) -> ([b], n, m)
mapValues f (a : as, n, m) = (b : bs, n'', m'')
 where
  (b , n' , m' ) = f a n m
  (bs, n'', m'') = mapValues f (as, n', m')
mapValues _ ([], n, m) = ([], n, m)

mapValuesResult
  :: (a -> n -> m -> Result e (b, n, m)) -> ([a], n, m) -> Result e ([b], n, m)
mapValuesResult _ ([]    , n, m) = Success ([], n, m)
mapValuesResult f (a : as, n, m) = do
  (b , n' , m' ) <- f a n m
  (bs, n'', m'') <- mapValuesResult f (as, n', m')
  return (b : bs, n'', m'')

name :: Int -> String
name x = "v" ++ show x

translateBinding
  :: Binding -> SSACtr -> VarMap -> Result String (Z.Binding, SSACtr, VarMap)
translateBinding (Binding _name args body) n m =
  mapValue (Z.Binding _name args') <$> translateExpr body n' m'
 where
  (args', n', m') =
    mapValues (\arg _n _m -> (name _n, _n + 1, (arg, _n) : _m)) (args, n, m)

translateExpr
  :: Expr -> SSACtr -> VarMap -> Result String (Z.Expr, SSACtr, VarMap)
translateExpr (Seq a pat b) n m = do
  (a'             , n'  , m'  ) <- translateExpr a n m
  ((pat', pre_pat), n'' , m'' ) <- translateVal Write pat n' m'
  (b'             , n''', m''') <- translateExpr b n'' m''
  return (apply pre_pat $ Z.Seq (Z.Expr a') pat' b', n''', m''')
translateExpr (Case v pats) n m = do
  ((v', pre_v), n' , m' ) <- translateVal Read v n m
  (pats'      , n'', m'') <- mapValuesResult translate_match (pats, n', m')
  return (apply pre_v $ Z.Case v' pats', n'', m'')
 where
  translate_match (pat, expr) _n _m = do
    (pat' , n' , m' ) <- translateCPat pat _n _m
    (expr', n'', m'') <- translateExpr expr n' m'
    return ((pat', expr'), n'', m'')
translateExpr (Ap f args) n m = do
  ((args', pre_args), n', m') <- mapValue unzip
    <$> mapValuesResult (translateVal Read) (args, n, m)
  ((args'', pre_args'), n'', m'') <- Success $ toSVal (args', n', m')
  return
    ( apply (concat pre_args) . apply pre_args' . Z.SExpr $ Z.Ap f args''
    , n''
    , m''
    )
translateExpr (Store val) n m = do
  ((val', pre_val), n', m') <- translateVal Read val n m
  return (apply pre_val (Z.SExpr $ Z.Store val'), n', m')
translateExpr (Fetch _name) n m = case lookup _name m of
  Nothing -> Error $ "'" ++ _name ++ "' is not defined. (Fetch)"
  Just x  -> Success (Z.SExpr $ Z.Fetch (name x), n, m)
translateExpr (Update _name val) n m = case lookup _name m of
  Nothing -> Error $ "'" ++ _name ++ "' is not defined. (Update)"
  Just x  -> do
    ((val', pre_val), n', m') <- translateVal Read val n m
    return (apply pre_val . Z.SExpr $ Z.Update (name x) val', n', m')
translateExpr (Unit val) n m = do
  ((val', pre_val), n', m') <- translateVal Read val n m
  return (apply pre_val . Z.SExpr $ Z.Unit val', n', m')

data Access = Write | Read
translateVal
  :: Access
  -> Val
  -> SSACtr
  -> VarMap
  -> Result String ((Z.Val, [Z.Expr -> Z.Expr]), SSACtr, VarMap)
translateVal a (CTag c vals) n m = do
  ((vals', pre_vals), n', m') <- mapValue unzip
    <$> mapValuesResult (translateVal a) (vals, n, m)
  ((vals'', pre_vals'), n'', m'') <- Success $ toSVal (vals', n', m')
  return ((Z.CTag c vals'', concat pre_vals ++ pre_vals'), n'', m'')
translateVal a (VTag v vals) n m = case lookup v m of
  Nothing -> Error $ "'" ++ v ++ "' is not defined. (VTag)"
  Just x  -> do
    ((vals', pre_vals), n', m') <- mapValue unzip
      <$> mapValuesResult (translateVal a) (vals, n, m)
    ((vals'', pre_vals'), n'', m'') <- Success $ toSVal (vals', n', m')
    return
      ((Z.VTag (name x) vals'', concat pre_vals ++ pre_vals'), n'' + 1, m'')
translateVal Read  (Tag c  ) n m = Success ((Z.Tag c, []), n, m)
translateVal Read  (Empty  ) n m = Success ((Z.Empty, []), n, m)
translateVal Write (Empty  ) n m = Success ((Z.Empty, []), n, m)
translateVal Read  (Lit lit) n m = Success ((Z.SVal (Z.Lit lit), []), n, m)
translateVal Write (Var v) n m =
  Success ((Z.SVal . Z.Var $ name n, []), n + 1, (v, n) : m)
translateVal Read (Var v) n m = case lookup v m of
  Nothing -> Error $ "'" ++ v ++ "' is not defined. (Var)"
  Just x  -> Success ((Z.SVal . Z.Var $ name x, []), n, m)
translateVal Write v _ _ = Error $ "Cannot write to " ++ show v


translateCPat
  :: CPat -> SSACtr -> VarMap -> Result String (Z.CPat, SSACtr, VarMap)
translateCPat (CPatCTag c vars) n m =
  mapValue (\vs -> Z.CPatCTag c vs) <$> vars'
 where
  vars' = mapValuesResult translate_var (vars, n, m)
  translate_var v _n _m = Success (name _n, _n + 1, (v, _n) : _m)
translateCPat (CPatTag c  ) n m = Success (Z.CPatTag c, n, m)
translateCPat (CPatLit lit) n m = Success (Z.CPatLit lit, n, m)
translateCPat (CPatVar var) n m =
  Success (Z.CPatVar (name n), n + 1, (var, n) : m)

save :: (Z.Val, SSACtr, VarMap) -> ((String, Z.Expr -> Z.Expr), SSACtr, VarMap)
save (v, n, m) =
  ((name n, Z.Seq (Z.Store v) (Z.SVal $ Z.Var (name n))), n + 1, m)

toSVal
  :: ([Z.Val], SSACtr, VarMap)
  -> (([Z.SVal], [Z.Expr -> Z.Expr]), SSACtr, VarMap)
toSVal ((Z.SVal (Z.Lit lit) : as), n, m) = ((Z.Lit lit : as', pres), n', m')
  where ((as', pres), n', m') = toSVal (as, n, m)
toSVal ((Z.SVal (Z.Var v) : as), n, m) = ((Z.Var v : as', pres), n', m')
  where ((as', pres), n', m') = toSVal (as, n, m)
toSVal ((v : as), n, m) = ((Z.Var v' : as', pre : pres), n'', m'')
 where
  ((v' , pre ), n' , m' ) = save (v, n, m)
  ((as', pres), n'', m'') = toSVal (as, n', m')
toSVal ([], n, m) = (([], []), n, m)

addEval :: [Z.Binding] -> [Z.Binding]
addEval bindings = eval : wrap_main (map useEval bindings)
 where
  eval = generateEval $ map (\(Z.Binding n args _) -> (n, args)) bindings
  wrap_main ((Z.Binding "main" [] body) : bs) =
    (Z.Binding
        "main"
        []
        (Z.Seq (Z.Expr body)
               (Z.SVal $ Z.Var "main_toeval")
               (Z.SExpr $ Z.Ap "#eval" [Z.Var "main_toeval"])
        )
      )
      : bs
  wrap_main (b : bs) = b : wrap_main bs
  wrap_main []       = []

generateEval :: [(String, [String])] -> Z.Binding
generateEval fs = Z.Binding
  "#eval"
  ["x"]
  (Z.Seq
    (Z.Fetch "x")
    (Z.SVal $ Z.Var "v")
    (Z.Case
      (Z.SVal $ Z.Var "v")
      (  match_fs fs
      ++ match_fs
           [ ("intAdd"  , ["a", "b"])
           , ("intPrint", ["p"])
           , ("intGT"   , ["c", "d"])
           , ("showHeap", [])
           , ("error"   , [])
           ]
      ++ [(Z.CPatVar "_", Z.SExpr $ Z.Unit (Z.SVal $ Z.Var "v"))]
      )
    )
  )
 where
  match_fs _fs = zip (map pat _fs) (map body _fs)
  pat (n, args) = (Z.CPatCTag n $ map (++ "_pat") args)
  body (n, args) =
    (Z.Seq
      (Z.Ap n $ map (Z.Var . (++ "_pat")) args)
      (Z.SVal $ Z.Var $ n ++ "_res")
      (Z.Seq (Z.Update "x" (Z.SVal $ Z.Var $ n ++ "_res"))
             (Z.Empty)
             (Z.SExpr $ Z.Ap "#eval" [Z.Var $ n ++ "_res"])
      )
    )

useEval :: Z.Binding -> Z.Binding
useEval (Z.Binding bname args body) =
  Z.Binding bname args . eval_args . delay $ body
 where
  delay (Z.Seq sexpr pat b) | allows_delay pat b =
    Z.Seq (delay_sexpr sexpr) pat (delay b)
  delay (Z.Seq sexpr pat b) = Z.Seq -- results of an application needed later have
    (Z.Expr $ Z.Seq
      sexpr           -- to be evaluated first
      (Z.SVal $ Z.Var $ "evalpat" ++ concat (get_vars pat))
      (Z.SExpr $ Z.Ap "#eval" [Z.Var $ "evalpat" ++ concat (get_vars pat)])
    )
    pat
    (eval_nontrivial pat $ delay b)
  delay (Z.Case v ms          ) = Z.Case v $ map delay_match ms -- TODO eval nontrivial cpat vars
  delay (Z.SExpr (Z.Expr expr)) = delay expr -- simplification
  delay (Z.SExpr sexpr        ) = Z.SExpr (delay_sexpr sexpr)

  delay_sexpr (Z.Ap n as  ) = Z.Store (Z.CTag n as)
  delay_sexpr (Z.Expr expr) = Z.Expr $ delay expr
  delay_sexpr other         = other

  delay_match (pat, expr) =
    (pat, apply (map eval_var (cpat_get_vars pat)) (use_eval_vars (delay expr)))

  eval_nontrivial (Z.CTag _ svals) expr =
    apply (map eval_var (svals_get_vars svals)) (use_eval_vars expr)
  eval_nontrivial (Z.VTag _ svals) expr =
    apply (map eval_var (svals_get_vars svals)) (use_eval_vars expr)
  eval_nontrivial _ expr = expr -- trivial

  allows_delay pat expr =
    null (vars_used (get_vars pat) expr) && pat_trivial pat

  vars_used vars (Z.Seq sexpr pat expr) =
    let pattern_requirements =
            if not (pat_trivial pat && null (vars_used (get_vars pat) expr))
              then sexpr_vars_used vars sexpr
              else []
    in  pattern_requirements ++ vars_used vars expr
  vars_used vars (Z.Case val bs) =
    get_vars val `overlapping` vars ++ concat (map (vars_used vars . snd) bs)
  vars_used vars (Z.SExpr sexpr) = sexpr_vars_used vars sexpr

  sexpr_vars_used vars (Z.Ap _ svals) = svals_get_vars svals `overlapping` vars
  sexpr_vars_used _    (Z.Unit  _     ) = [] -- There's no problem with delaying returned values
  sexpr_vars_used vars (Z.Store val   ) = get_vars val `overlapping` vars
  sexpr_vars_used vars (Z.Fetch v     ) = if v `elem` vars then [v] else []
  sexpr_vars_used vars (Z.Update _ val) = get_vars val `overlapping` vars
  sexpr_vars_used vars (Z.Expr expr   ) = vars_used vars expr

  pat_trivial (Z.SVal (Z.Var _)) = True
  pat_trivial (Z.Empty         ) = True
  pat_trivial _                  = False

  get_vars (Z.CTag _  svals ) = svals_get_vars svals
  get_vars (Z.VTag vt svals ) = vt : svals_get_vars svals
  get_vars (Z.SVal (Z.Var v)) = [v]
  get_vars _                  = []

  svals_get_vars = mapMaybe
    (\x -> case x of
      Z.Lit _ -> Nothing
      Z.Var v -> Just v
    )

  cpat_get_vars (Z.CPatCTag _ vars) = vars
  cpat_get_vars _                   = []

  overlapping (x : xs) y = if x `elem` y then [x] else [] ++ xs `overlapping` y
  overlapping []       _ = []

  eval_args b = apply (map eval_var args) (use_eval_vars b)

  eval_var var =
    Z.Seq (Z.Ap "#eval" [Z.Var var]) (Z.SVal $ Z.Var $ var ++ "_eval")

  use_eval_vars (Z.Seq sexpr pat expr) =
    Z.Seq (sexpr_use_eval_vars sexpr) pat (use_eval_vars expr)
  use_eval_vars (Z.Case v ms) =
    Z.Case (val_use_eval_vars v) (map (second use_eval_vars) ms)
  use_eval_vars (Z.SExpr sexpr) = Z.SExpr $ sexpr_use_eval_vars sexpr

  sexpr_use_eval_vars (Z.Ap v svals) = Z.Ap v (map sval_use_eval_vars svals)
  sexpr_use_eval_vars (Z.Unit  val ) = Z.Unit (val_use_eval_vars val)
  sexpr_use_eval_vars (Z.Store val ) = Z.Store (val_use_eval_vars val)
  sexpr_use_eval_vars (Z.Fetch var ) = Z.Fetch (var_use_eval_vars var)
  sexpr_use_eval_vars (Z.Update var val) =
    Z.Update (var_use_eval_vars var) (val_use_eval_vars val)
  sexpr_use_eval_vars (Z.Expr expr) = Z.Expr (use_eval_vars expr)

  val_use_eval_vars (Z.CTag c svals) = Z.CTag c (map sval_use_eval_vars svals)
  val_use_eval_vars (Z.VTag v svals) =
    Z.VTag (var_use_eval_vars v) (map sval_use_eval_vars svals)
  val_use_eval_vars (Z.SVal sval) = Z.SVal (sval_use_eval_vars sval)
  val_use_eval_vars other         = other

  sval_use_eval_vars (Z.Lit lit) = Z.Lit lit
  sval_use_eval_vars (Z.Var v  ) = Z.Var $ var_use_eval_vars v

  var_use_eval_vars var = if var `elem` args then var ++ "_eval" else var

addApply :: [Z.Binding] -> [Z.Binding]
addApply = id -- TODO support partial application

example :: IRY
example = IRY
  [ (Binding
      "main"
      []
      (Seq
        (Ap "upto" [CTag "#C#Int" [Lit "1"], CTag "#C#Int" [Lit "20"]])
        (Var "list")
        (Seq (Ap "sum" [Var "list"])
             (Var "result")
             (Ap "intPrint" [Var "result"])
        )
      )
    )
  , (Binding
      "upto"
      ["m", "n"]
      (Seq
        (Ap "intGT" [Var "m", Var "n"])
        (Var "b")
        (Case
          (Var "b")
          [ (CPatTag "CTrue", (Unit (Tag "CNil")))
          , ( CPatTag "CFalse"
            , (Seq
                (Ap "intAdd" [Var "m", Lit "1"])
                (Var "m'")
                (Seq (Ap "upto" [Var "m'", Var "n"])
                     (Var "xs")
                     (Unit (CTag "CCons" [Var "m'", Var "xs"]))
                )
              )
            )
          ]
        )
      )
    )
  , (Binding
      "sum"
      ["l"]
      (Case
        (Var "l")
        [ (CPatTag "CNil", Unit (CTag "#C#Int" [Lit "0"]))
        , ( CPatCTag "CCons" ["t", "ts"]
          , (Seq
              (Ap "sum" [Var "ts"])
              (CTag "#C#Int" [Var "r"])
              (Seq (Ap "intAdd" [Var "t", Var "r"])
                   (Var "res")
                   (Unit (Var "res"))
              )
            )
          )
        ]
      )
    )
  ]

translated :: IRY
translated = IRY
  [ Binding
    "main"
    []
    (Seq
      (Seq
        (Store (Lit "1"))
        (Var "#a0")
        (Seq (Store (Lit "2")) (Var "#a1") (Ap "upto" [Var "#a0", Var "#a1"]))
      )
      (Var "#a2")
      (Unit (Var "#a2"))
    )
  , Binding
    "upto"
    ["a", "b"]
    (Seq
      (Seq
        (Seq
          (Store (Var "a"))
          (Var "#a2")
          (Seq (Store (Var "b")) (Var "#a3") (Ap "intGT" [Var "#a2", Var "#a3"])
          )
        )
        (Var "#a4")
        (Case
          (Var "#a4")
          [ (CPatTag "CTrue", Store (Tag "CNil"))
          , ( CPatTag "CFalse"
            , Seq
              (Store (Var "a"))
              (Var "#a5")
              (Seq
                (Seq
                  (Seq
                    (Store (Var "a"))
                    (Var "#a6")
                    (Seq (Store (Lit "1"))
                         (Var "#a7")
                         (Ap "intAdd" [Var "#a6", Var "#a7"])
                    )
                  )
                  (Var "#a8")
                  (Seq (Store (Var "b"))
                       (Var "#a9")
                       (Ap "upto" [Var "#a8", Var "#a9"])
                  )
                )
                (Var "#a10")
                (Store (CTag "CCons" [Var "#a5", Var "#a10"]))
              )
            )
          ]
        )
      )
      (Var "#a11")
      (Unit (Var "#a11"))
    )
  ]
