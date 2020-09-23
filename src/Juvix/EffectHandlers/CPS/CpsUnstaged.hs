module Juvix.EffectHandlers.CPS.CpsUnstaged where

newtype Cps r a = Cps { unCps :: (a -> r) -> r}

shift0 :: ((a -> r) -> r) -> Cps r a
shift0 = id

runIn0 :: Cps r a -> (a -> r) -> r
runIn0 = id

reset0 :: Cps r r -> r
reset0 m = runIn0 m id

push :: (a -> Cps r b) -> (b -> r) -> (a -> r)
push f k = \a -> f a k

bind :: Cps r a -> (a -> Cps r b) -> Cps r b
bind m f = \k -> m (push f k)

instance Functor (Cps r) where
  fmap = push

instance Applicative (Cps r) where
  pure a  k = k a
  (<*>) = undefined

instance Monad (Cps r) where
  return = pure
  m >>= f = bind m f

example :: Int
example = 1 + reset0 (do
  x <- shift0 (\k -> k (k 100))
  pure (10 + x))
