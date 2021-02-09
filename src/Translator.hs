{-# LANGUAGE MultiParamTypeClasses #-}
module Translator where

import           Util

class Translatable a b where
  translate :: a -> Result String b
