module SP where

import Arrow

data SP a b = Put b (SP a b) | Get (a -> SP a b)

runSP :: SP a b -> [a] -> [b]
runSP (Put b s) as = b : runSP s as
runSP (Get k) (a:as) = runSP (k a) as
runSP (Get _) [] = []

instance Arrow SP where
    arr f = Get (\ a -> Put (f a) (arr f))

    sp1 >>> Put b sp2 = Put b (sp1 >>> sp2)
    Put b sp1 >>> Get f = sp1 >>> f b
    Get f >>> sp1 = Get (\ a -> f a >>> sp1)

    first = bypass []
        where bypass (c : cs) (Put b sp) = Put (b, c) (bypass cs sp)
              bypass cs (Get f) = Get (\ (a, c) -> bypass (c : cs) (f a))
              bypass [] (Put b sp) = Get ( \ (a, c) -> Put (b , c) (bypass [] sp))

instance ArrowChoice SP where
    left (Put b sp1) = Put (Left b) (left sp1)
    left (Get f) = Get (either undefined undefined)
