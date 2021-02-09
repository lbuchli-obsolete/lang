module IR.IRX where

newtype IRX = IRX [Binding] deriving Show
type Const = String
type Lit = String
type Name = String
data Binding = Binding Name [Name] Expr deriving Show
data Expr = Seq Expr Pat Expr
          | Case Expr [(Pat, Expr)]
          | Ap Name [Expr]
          | CTag Const [Expr]
          | Empty
          | Lit Lit
          | Var Name
 deriving Show
data Pat = PatVar Name
         | PatCTag Const [Pat]
         | PatLit Lit
         | PatEmpty
 deriving Show
