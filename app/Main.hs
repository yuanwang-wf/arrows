module Main where

import Arrow
import SF


streamFns = runSF (arr (+1)) [1..5]
pairPred = arr id &&& delay 0

main :: IO ()
main = print streamFns