module Circuits where

import Arrow

class ArrowLoop a => ArrowCircuit a where
    delay :: b -> a b b

nor :: Arrow a => a (Bool, Bool) Bool
nor = arr (not . uncurry (||))