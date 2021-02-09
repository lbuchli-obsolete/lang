module Main where

import           Lib

main :: IO ()
main = -- print $ interpret example2
  print $ translate example >>= interpret
  -- print $ interpret example2
