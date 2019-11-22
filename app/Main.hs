module Main where

import           BF

main :: IO (Mem Int)
main = run [Next, Inc, Out]
