module Lib
  ( interpret
  , X.example
  , Lib.translate
  , copyResult
  , parse
  )
where

import qualified IR.IRW                        as W
import qualified IR.IRX                        as X
import qualified IR.IRY                        as Y
import qualified IR.IRZ                        as Z
import           Interpreter
import           Util
import           Parse


translate :: W.IRW -> Result String Z.IRZ
translate irw = Trace (show translationZ) translationZ
 where
  translationZ = translationY >>= Y.translate
  translationY = translationX >>= X.translate
  translationX = W.translate irw

{-
translate :: Y.IRY -> Result String Z.IRZ
translate iry = Y.translate iry
-}

copyResult :: Result String (Z.Val, Heap)
copyResult = copy
  [ ("v", Z.CTag "CCons" [Z.Var "a", Z.Var "b"])
  , ("a", Z.SVal $ Z.Lit "1")
  , ("b", Z.Tag "CNil")
  ]
  (Z.SVal $ Z.Var "v")
