module Parse
  ( parse
  )
where

import qualified Parse.ParserPass1             as P1
import qualified Parse.ParserPass2             as P2

import           IR.IRW
import           Util

parse :: String -> Result String IRW
parse str = P1.parse str >>= \p1 -> Trace (show p1) $ P2.parse p1 str
