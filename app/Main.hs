module Main where

import Control.Applicative
import Control.Monad (liftM)
import Arrow
import SF

count :: String -> String -> Int
count w = length . filter (== w) . words

countM w = fmap (length  . filter (== w) . words) . readFile


streamFns = runSF (mapA (delay 0)) [[1,2,3], [4,5], [6], [7,8], [9, 10, 11], [12,13,14,15]]
streamFns' = runSF delaysA [[1,2,3], [4,5,6], [7,8, 9], [10, 11, 12]]
pairPred = arr id &&& delay 0

nor :: SF (Bool, Bool) Bool
nor = arr (not . uncurry (||))

main :: IO ()
main = print streamFns