module Main where

import           Lib
import           System.IO

file :: String
file = "/home/lukas/workspace/haskell/lang/lang-src/devsrc"

main :: IO ()
main = do
  handle   <- openFile file ReadMode
  contents <- hGetContents handle
  print $ parse contents >>= translate >>= interpret
