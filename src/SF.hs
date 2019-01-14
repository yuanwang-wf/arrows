module SF where

newtype SF a b = SF {runSF :: [a] -> [b]}

delay :: a -> SF a a
delay x = SF (x :)