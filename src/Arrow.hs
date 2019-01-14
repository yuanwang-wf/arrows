module Arrow where

import Kleisli
import SF

class Arrow arr where
    arr   :: (a -> b) -> arr a b
    (>>>) :: arr a b -> arr b c -> arr a c

    (&&&) :: arr a b -> arr a c -> arr a (b, c)
    f &&& g = arr (\ x -> (x, x)) >>> (f *** g)

    (***) :: arr a b -> arr c d -> arr (a, c) (b, d)
    f *** g = first f >>> second g

    first :: arr a b -> arr (a, c) (b, c)

    second :: arr a b -> arr (c, a) (c ,b)
    second f = arr swap >>> first f >>> arr swap
        where swap :: (a, b) -> (b, a)
              swap (a, b) = (b, a)

class Arrow arr => ArrowChoice arr where
    (|||) :: arr a c -> arr b c -> arr (Either a b) c

-- mapA :: ArrowChoice arr => arr a b -> arr [a] [b]
-- mapA f = arr listcase >>> (arr (const []) ||| ((f *** mapA) >>> arr (uncurry  (:))))
--     where listcase :: [a] -> Either () (a, [a])
--           listcase []     = Left ()
--           listcase (x:xs) = Right (x, xs)

ifte :: Arrow arr => arr a Bool -> arr a b -> arr a b -> arr a b
ifte p f g = undefined


instance Arrow (->) where
    arr   = id
    (>>>) = flip (.)
    -- f &&& g = \ x -> (f x, g x)
    -- f *** g = \ (a, b) -> (f a, g b)
    first f (a, c) = (f a, c)

instance ArrowChoice (->) where
    (|||) = either

instance Monad m => Arrow (Kleisli m) where
    arr f = Kleisli (return . f)
    Kleisli f >>> Kleisli g = Kleisli $ \ a -> do x <- f a
                                                  g x
    -- Kleisli f &&& Kleisli g = Kleisli $ \ a -> do x <- f a
    --                                               y <- g a
    --                                               return (x, y)
    -- Kleisli f *** Kleisli g = Kleisli $ \ (a, b) -> do x <- f a
    --                                                    y <- g b
    --                                                    return (x, y)
    first (Kleisli f)       = Kleisli $ \ (a , c) -> do b <- f a
                                                        return (b, c)

instance Arrow SF where
    arr f = SF (map f)
    SF f >>> SF g = SF (f >>> g)
    -- SF f &&& SF g = SF (f &&& g >>> uncurry zip)
    -- SF f *** SF g = SF (\ x -> zip (f (map fst x ) ) (g (map snd x)))
    -- SF f *** SF g = SF (unzip >>> (f *** g) >>> uncurry zip)
    first (SF f)  = SF (unzip >>> first f >>> uncurry zip)