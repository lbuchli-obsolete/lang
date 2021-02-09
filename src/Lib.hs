module Lib
  ( interpret
  , example
  , example2
  , translate
  )
where

import qualified Translator
import           IR.IRY
import           Interpreter
import           Util
import           IR.IRZ                         ( IRZ
                                                , example2
                                                )


translate :: IRY -> Result String IRZ
translate iry = Trace (show translation) translation
  where translation = Translator.translate iry
