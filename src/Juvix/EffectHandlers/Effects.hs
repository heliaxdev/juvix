module Juvix.EffectHandlers.Effects where

import Juvix.EffectHandlers.CPS

example : Int
example = 1 + reset0 (do
  x <- shift0 (\k -> k (k 100))
  pure (10 + x))
