module Interpreter where

import           Util
import           IR.IRZ
import           Control.Applicative            ( (<|>) )
import           Data.Bifunctor                 ( first
                                                , second
                                                )

type Bindings = [(String, ([String], Expr))]
type Heap = [(String, Val)]

interpret :: IRZ -> Result String (Val, Heap)
interpret (IRZ bs) = case lookup "main" bindings of
  Nothing            -> Error "No main found."
  Just (_ : _, _   ) -> Error "The main function cannot have arguments."
  Just ([]   , body) -> reduceExpr bindings heap body
 where
  bindings = map (\(Binding name vars body) -> (name, (vars, body))) bs
  heap     = []

reduceExpr :: Bindings -> Heap -> Expr -> Result String (Val, Heap)
reduceExpr bs h (Seq se pat e) = do
  (se', h') <- reduceSExpr bs h se
  h''       <- intoVal bs h' se' pat
  result    <- reduceExpr bs h'' e
  return result
reduceExpr bs h (Case v pats) =
  check (map (first (intoCPat bs h v)) pats) >>= match
 where
  match (h', body) = reduceExpr bs h' body
  check ((x, b) : xs) = ((\x' -> (x', b)) <$> x) <|> check xs
  check []            = Error "No case matches."
reduceExpr bs h (SExpr se) = reduceSExpr bs h se
reduceSExpr :: Bindings -> Heap -> SExpr -> Result String (Val, Heap)
reduceSExpr bs h (Ap "intAdd__" [a, b]) = do
  a' <- getInt intTries bs h a
  b' <- getInt intTries bs h b
  return (CTag "#C#Int" [Lit $ show $ a' + b'], h)
reduceSExpr bs h (Ap "intGT__" [a, b]) = do
  a' <- getInt intTries bs h a
  b' <- getInt intTries bs h b
  return (Tag $ if a' > b' then "CTrue" else "CFalse", h)
reduceSExpr _ h (Ap "showHeap" []) = Trace (show h) $ Success (Empty, h)
reduceSExpr bs h (Ap "intPrint_" [x]) =
  getInt intTries bs h x >>= \x' -> Trace (show x') $ Success (Empty, h)
reduceSExpr bs h (Ap "strLength_" [s]) = do
  s' <- getStr strTries bs h s
  return (CTag "#C#Int" [Lit $ show $ length s'], h)
reduceSExpr bs h (Ap "strConcat__" [a, b]) = do
  a' <- getStr strTries bs h a
  b' <- getStr strTries bs h b
  return (CTag "#C#Str" [Lit $ a' ++ b'], h)
reduceSExpr bs h (Ap "strTake__" [x, s]) = do
  x' <- getInt intTries bs h x
  s' <- getStr strTries bs h s
  return (CTag "#C#Str" [Lit $ take x' s'], h)
reduceSExpr bs h (Ap "strDrop__" [x, s]) = do
  x' <- getInt intTries bs h x
  s' <- getStr strTries bs h s
  return (CTag "#C#Str" [Lit $ drop x' s'], h)
reduceSExpr _ _ (Ap "error" []) = error "Program invoked error."
reduceSExpr bs h (Ap f args) =
  Trace ("Calling " ++ f ++ " :: " ++ show args) $ case lookup f bs of
    Nothing -> Error $ "Function '" ++ f ++ "' not found."
    Just (vars, body) ->
      setVars args vars >>= \h' -> second (++ h) <$> reduceExpr bs h' body
 where
  setVars ((Var a) : as) (v : vs) = case lookup a h of
    Nothing ->
      Error $ "Argument '" ++ a ++ "' not found. (Heap: " ++ show h ++ ")"
    Just x -> do
      (x', h') <- copy h x
      h''      <- setVars as vs
      return (alloc v x' h' ++ h'')
  setVars ((Lit a) : as) (v : vs) = alloc v (SVal $ Lit a) <$> setVars as vs
  setVars []             (_ : _ ) = Error "Not enough arguments."
  setVars (_ : _)        []       = Error "Too many arguments."
  setVars []             []       = Success []
reduceSExpr _ h (Unit (SVal (Var val))) = case lookup val h of
  Just x  -> let result = copy h x in Trace ("Returning ") $ result
  Nothing -> Error $ "Variable '" ++ val ++ "' could not be found. (Unit)"
reduceSExpr _ h (Unit val) = Trace ("Returning ") $ result
  where result = copy h val
reduceSExpr _ h (Store (SVal (Var val))) = case lookup val h of
  Just x  -> Success (x, h)
  Nothing -> Error $ "Variable '" ++ val ++ "' could not be found. (Store)"
reduceSExpr _ h (Store val) = Success (val, h)
reduceSExpr _ h (Fetch var) = case lookup var h of
  Just x  -> Success (x, h)
  Nothing -> Error $ "Variable '" ++ var ++ "' could not be found. (Fetch)"
reduceSExpr bs h (Update var (SVal (Var val))) = case lookup val h of
  Just x  -> reduceSExpr bs h (Update var x)
  Nothing -> Error $ "Variable '" ++ var ++ "' could not be found. (Update)"
reduceSExpr _ h (Update var val) = (\h' -> (Empty, h')) <$> update h
 where
  update ((n, _) : hs) | n == var = Success (alloc n val hs)
  update ((k, v) : hs) = (alloc k v) <$> update hs
  update [] = Error $ "Cannot update '" ++ var ++ "': Not found in heap."
reduceSExpr bs h (Expr expr) = reduceExpr bs h expr

intoVal :: Bindings -> Heap -> Val -> Val -> Result String Heap
intoVal _ h (CTag fc fvs) (CTag tc tvs) | fc == tc = fvs `into` tvs
 where
  into (Lit fv : fs) (Lit tv : ts) | fv == tv = fs `into` ts
  into (Lit fv : fs) (Var tv : ts) =
    (alloc tv (SVal $ Lit fv)) <$> fs `into` ts
  into (Var fv : fs) (Lit tv : ts) = case lookup fv h of
    Just (SVal (Lit fv')) | fv' == tv -> fs `into` ts
    Just x -> Error $ "'" ++ show x ++ "' does not match '" ++ tv ++ "'"
    Nothing -> Error $ "Variable '" ++ fv ++ "' not found. (Var->Lit, CTag)"
  into (Var fv : fs) (Var tv : ts) = case lookup fv h of
    Just x  -> (alloc tv x) <$> fs `into` ts
    Nothing -> Error $ "Variable '" ++ fv ++ "' not found. (Var->Var, CTag)"
  into []      (_ : _) = Error "Pattern has too many values. (CTag)"
  into (_ : _) []      = Error "Pattern has not enough values. (CTag)"
  into []      []      = Success h
  into _       _       = Error "Pattern does not match. (CTag)"
intoVal _ _ (VTag _ _) _ = Error "VTags are currently not in use (TODO)"
intoVal _ _ _ (VTag _ _) = Error "VTags are currently not in use (TODO)"
intoVal _ h (Tag fc) (Tag tc) | fc == tc = Success h
intoVal _ h Empty Empty = Success h
intoVal _ h (SVal fv) (SVal tv) =
  (\h' -> apply (map (\(k, v) -> alloc k v) h') h) <$> fv `into` tv
 where
  into (Lit f) (Lit t) | f == t = Success []
  into (Lit f) (Var t)          = Success [(t, SVal $ Lit f)]
  into (Var f) (Var t)          = case lookup f h of
    Just x  -> Success [(t, x)]
    Nothing -> Error $ "Variable '" ++ f ++ "' not found. (Var->Var, SVal)"
  into (Var f) (Lit t) = case lookup f h of
    Just (SVal (Lit f')) | f' == t -> Success []
    Just x -> Error $ "'" ++ show x ++ "' does not match '" ++ t ++ "'"
    Nothing -> Error $ "Variable '" ++ f ++ "' not found. (Var->Lit, CTag)"
  into _ _ = Error "Pattern does not match. (SVal)"
intoVal _ h fv (SVal (Var tv)) = Success (alloc tv fv h)
intoVal _ _ val pat =
  Error
    $  "Pattern "
    ++ show pat
    ++ " does not match value "
    ++ show val
    ++ ". (intoVal)"

intoCPat :: Bindings -> Heap -> Val -> CPat -> Result String Heap
intoCPat _ h (CTag fc fvs) (CPatCTag tc tvs) | fc == tc = fvs `into` tvs
 where
  into (Lit lit : fs) (v : ts) = alloc v (SVal $ Lit lit) <$> fs `into` ts
  into (Var fv  : fs) (v : ts) = case lookup fv h of
    Just x  -> alloc v x <$> fs `into` ts
    Nothing -> Error $ "Variable '" ++ fv ++ "' not found. (Var, CTag, CPat)"
  into []      (_ : _) = Error "Pattern has too many values. (CPat)"
  into (_ : _) []      = Error "Pattern has not enough values. (CPat)"
  into []      []      = Success h
intoCPat _ h (Tag fc) (CPatTag tc) | fc == tc    = Success h
intoCPat _ h (SVal (Lit f)) (CPatLit t) | f == t = Success h
intoCPat _  h val            (CPatVar v)         = Success $ alloc v val h
intoCPat bs h (SVal (Var f)) pat                 = case lookup f h of
  Just x  -> intoCPat bs h x pat
  Nothing -> Error $ "Variable '" ++ f ++ "' not found. (Var, CPat)"
intoCPat _ h val pat =
  Error
    $  "Pattern "
    ++ show pat
    ++ " does not match value "
    ++ show val
    ++ ". (CPat), "
    ++ show h

copy :: Heap -> Val -> Result String (Val, Heap)
copy h (CTag c svals) = first (CTag c) <$> copySVals h svals
copy h (VTag v svals) = first (VTag v) <$> copySVals h svals -- TODO variable should also be copied
copy h (SVal sval   ) = first SVal <$> copySVal h sval
copy _ other          = Success (other, [])

copySVal :: Heap -> SVal -> Result String (SVal, Heap)
copySVal _ (Lit lit) = Trace "SVal copy lit" $ Success (Lit lit, [])
copySVal h (Var var) = Trace "SVal copy var" $ case lookup var h of
  Just x ->
    first Var . (\(v, h') -> second (++ h') $ allocCpy var v h) <$> copy h x
  Nothing ->
    Error
      $  "Variable "
      ++ var
      ++ " not available for copy (Heap: "
      ++ show h
      ++ ")"

copySVals :: Heap -> [SVal] -> Result String ([SVal], Heap)
copySVals h svals =
  second concat . unzip <$> (sequence $ map (copySVal h) svals)

intTries :: Int
intTries = 100

getInt :: Int -> Bindings -> Heap -> SVal -> Result String Int
getInt _ _ _ (Lit x)                  = Success $ read x
getInt tries bs h (Var v) | tries > 0 = case lookup v h of
  Just (CTag "#C#Int" [x]) -> getInt (tries - 1) bs h x
  Just (SVal x           ) -> getInt (tries - 1) bs h x
  Just _                   -> try_eval
  Nothing -> Error $ "Could not find variable '" ++ v ++ "'. (getInt)"
 where
  try_eval = reduceSExpr bs h (Ap "#eval" [Var v]) >>= \(val, h') ->
    case val of
      (CTag "#C#Int" [x]) -> getInt (tries - 1) bs h x
      (SVal sval) -> getInt (tries - 1) bs h' sval
      x -> Error $ "This doesn't seem to be an int: " ++ show x
getInt _ _ _ v = Error ("getInt tries exhausted on: " ++ show v)

strTries :: Int
strTries = 100

getStr :: Int -> Bindings -> Heap -> SVal -> Result String String
getStr _ _ _ (Lit x)                  = Success x
getStr tries bs h (Var v) | tries > 0 = case lookup v h of
  Just (CTag "#C#Str" [x]) -> getStr (tries - 1) bs h x
  Just (SVal x           ) -> getStr (tries - 1) bs h x
  Just _                   -> try_eval
  Nothing -> Error $ "Could not find variable '" ++ v ++ "'. (getStr)"
 where
  try_eval = reduceSExpr bs h (Ap "#eval" [Var v]) >>= \(val, h') ->
    case val of
      (CTag "#C#Str" [x]) -> getStr (tries - 1) bs h x
      (SVal sval) -> getStr (tries - 1) bs h' sval
      x -> Error $ "This doesn't seem to be a string: " ++ show x
getStr _ _ _ v = Error ("getStr tries exhausted on: " ++ show v)

alloc :: Var -> Val -> Heap -> Heap
alloc k (SVal (Var v)) h = case lookup v h of
  Nothing -> error "Heap discrepancy"
  Just x  -> alloc k x h
alloc k v h = case lookup k h of
  Nothing -> (k, v) : h
  Just _  -> map replaceMatch h
 where
  replaceMatch (k', _) | k == k' = (k', v)
  replaceMatch (k', v')          = (k', v')

allocCpy :: Var -> Val -> Heap -> (Var, Heap)
allocCpy k v h = case lookup k h of
  Nothing -> (k, (k, v) : [])
  Just _  -> allocCpy (k ++ "'") v h
