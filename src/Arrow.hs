module Arrow where

import Control.Applicative (liftA2)
import Control.Monad (liftM)
import Control.Monad.Fix
import Kleisli
import SF


infixr 3 ***
infixr 3 &&&
infixr 2 +++
infixr 2 |||
infixr 1 >>>

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
    f ||| g = f +++ g >>> arr join
        where join (Left b) = b
              join (Right b) = b

    (+++) :: arr a b -> arr c d -> arr (Either a c) (Either b d)
    f +++ g = left f >>> right g

    left :: arr a b -> arr (Either a c) (Either b c)

    right :: arr a b -> arr (Either c a) (Either c b)
    right f = arr mirror >>> left f >>> arr mirror
        where mirror (Left a) = Right a
              mirror (Right a) = Left a

mapA :: ArrowChoice arr => arr a b -> arr [a] [b]
mapA f = arr listcase >>>
         arr (const []) ||| (f *** mapA f >>> arr (uncurry (:)))
    where listcase []     = Left ()
          listcase (x:xs) = Right (x, xs)

delaysA = arr listcase >>>
           arr (const []) |||
           (arr id *** (delaysA >>> delay []) >>>
            arr (uncurry (:)))
    where listcase []     = Left ()
          listcase (x:xs) = Right (x, xs)

class Arrow arr => ArrowLoop arr where
    loop :: arr (a,c) (b,c) -> arr a b

class Arrow arr => ArrowApply arr where
    app :: arr (arr a b, a) b

newtype ArrowMonad arr a = ArrowMonad (arr () a)

instance Arrow a => Functor (ArrowMonad a) where
    fmap f (ArrowMonad m) = ArrowMonad (m >>> arr f)

instance Arrow a => Applicative (ArrowMonad a) where
    pure x = ArrowMonad (arr (const x))
    ArrowMonad x <*> ArrowMonad y = ArrowMonad (x &&& y >>> arr (\ (f, a) -> f a))

instance ArrowApply a => Monad (ArrowMonad a) where
    ArrowMonad m >>= f = ArrowMonad (m >>>
                                     arr (\ x -> let ArrowMonad h = f x in (h, ()))
                                     >>> app)

ifte :: ArrowChoice arr => arr a Bool -> arr a b -> arr a b -> arr a b
ifte p f g = p &&& arr id >>> arr h >>> f ||| g
    where h :: (Bool, a) -> Either a a
          h (True, a) = Left a
          h (False, a) = Right a

instance Arrow (->) where
    arr   = id
    (>>>) = flip (.)
    -- f &&& g = \ x -> (f x, g x)
    -- f *** g = \ (a, b) -> (f a, g b)
    first f (a, c) = (f a, c)

instance ArrowChoice (->) where
    (|||) = either
    f +++ g = either (Left . f) (Right . g)
    left f = either (Left .f) Right

instance ArrowLoop (->) where
    loop f a = b
        where (b , c) = f (a, c)

instance ArrowApply (->) where
    app (f, x) = f x

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

instance Monad m => ArrowChoice (Kleisli m) where
    Kleisli f ||| Kleisli g = Kleisli $ \ x -> either f g x
    Kleisli f +++ Kleisli g = Kleisli $ \ x -> either (fmap Left . f) (fmap Right .g) x
    left (Kleisli f) = Kleisli $ \ x -> either (fmap Left . f) (fmap Right . return) x

instance MonadFix m => ArrowLoop (Kleisli m) where
        loop (Kleisli f) = Kleisli (liftM fst . mfix . f')
          where f' x y = f (x, snd y)

instance Monad m => ArrowApply (Kleisli m) where
    app = Kleisli $ (\(Kleisli mb, a) -> mb a)

instance Arrow SF where
    arr f = SF (map f)
    SF f >>> SF g = SF (f >>> g)
    -- SF f &&& SF g = SF (f &&& g >>> uncurry zip)
    -- SF f *** SF g = SF (\ x -> zip (f (map fst x ) ) (g (map snd x)))
    -- SF f *** SF g = SF (unzip >>> (f *** g) >>> uncurry zip)
    first (SF f)  = SF (unzip >>> first f >>> uncurry zip)

instance ArrowChoice SF where
    --SF f ||| SF g = SF (map (either (\ l -> f [l]) (\ r -> g [r])) >>> concat)
    --SF f +++ SF g = SF (map (either (\ l -> map Left $ f [l]) (\ r -> map Right $ g [r])) >>> concat)
    --left (SF f) = SF (map (either (\ l -> map Left $ f [l]) (\ r -> map Right [r])) >>> concat)
    left (SF f) = SF (\ xs -> combine xs (f [y | Left y <- xs]))
        where combine (Left  y:xs) (z:zs) = Left z : combine xs zs
              combine (Right y:xs) zs     = Right y : combine xs zs
              combine []           _      = []


--------------exercises---------------
--- filter _ [] =
--- filter pred (x : xs)
--    | pref x = x : filter pred xs
--    | other  = filter pred xs

-- filterA :: ArrowChoice arr => arr a Bool -> arr [a] [a]
-- filterA f = arr listcase >>>
--             arr (const []) ||| (ifte f undefined (filterA f))
--     where listcase [] = Left ()
--           listcase (x: xs) = Right (x, xs)

--           h :: (Bool, a) -> Either () a
--           h (False, a) = Left ()
--           h (True, a) = Right a

--           arrB :: arr a Bool -> arr a [a]
--           arrB f = f &&& arr id >>> arr h