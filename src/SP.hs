module SP where
--http://hackage.haskell.org/package/streamproc-1.6.2/docs/Control-Arrow-SP.html
import Arrow
import Circuits

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
    left (Get f) = Get (\ eitherAC -> case eitherAC of
                                       Left a -> left (f a)
                                       Right c -> Put (Right c) (left (Get f)))

instance ArrowCircuit SP where
    delay b = Put b (arr id)

-- http://hackage.haskell.org/package/streamproc-1.6.2/docs/src/Control-Arrow-SP.html#SP
instance ArrowLoop SP where
    loop = loop' empty
        where
            loop' :: Queue c -> SP (a,c) (b,c) -> SP a b
            loop' q (Put (b,c) sp') = Put b $ loop' (push c q) sp'
            loop' q (Get f) = case pop q of
                Nothing -> error "invalid attempt to consume empty SP feedback loop"
                Just (c, q') -> Get (\ x -> loop' q' $ f (x, c))

data Queue a = Queue [a] [a]

empty :: Queue a
empty = Queue []  []

push :: a -> Queue a -> Queue a
push e (Queue o i) = Queue o (e:i)

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue [] []) = Nothing
pop (Queue (o:os) i) = Just (o, Queue os i)
pop (Queue [] i) = pop (Queue (reverse i)  [])