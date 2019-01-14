module Kleisli where

newtype Kleisli m a b = Kleisli {runKleisli :: a -> m b}