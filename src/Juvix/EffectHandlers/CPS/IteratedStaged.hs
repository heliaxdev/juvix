module Juvix.EffectHandlers.CPS.IteratedStaged where

import Juvix.EffectHandlers.CPS.CpsStaged
import Juvix.EffectHandlers.CPS.IteratedUnstaged

data SMT :: (* -> *) -> * -> * where
  Empty :: [] -> a -> SMT [] (Exp a)
  Inhabitated :: (r : rs) -> a -> (Exp a -> SMT rs r) -> SMT rs r

pure :: Exp a -> rs -> STM rs a
pure a [] = Empty [] a
pure a (r :: rs) = \k -> k a

push :: (Exp a -> STM (r :: rs) b) -> (Exp b -> STM rs r) -> (Exp a -> STM rs r)
push f k a = f a k

bind :: STM rs a -> (Exp a -> STM rs b) -> STM rs b
bind  m@(Empty _ _)  f = f m
bind  m@(Inhabitated _ _) f k = m (push f k)

(>>=) :: STM rs a -> (Exp a -> STM rs b) -> STM rs b
(>>=) = bind

lift :: STM rs a -> STM rs a
lift = bind

shift0 :: ((Exp a -> STM rs r) -> STM rs r) -> STM (r :: rs) a
shift0 = id

runIn0 :: STM (r :: rs) a -> (Exp a -> STM rs r) -> STM rs r
runIn0 = id

reset0 :: STM (a :: rs) a -> STM rs a
reset0 m = runIn0 m pure

reify :: STM rs a -> Exp (Stm rs a)
reify m@(Empty _ _) = m
reify m@(Inhabitated (q:qs) _)  =
 Lam $ \ k ->  reify (m (reflect . App k  a))

reflect :: Exp (Stm rs a) -> STM rs a
reflect exp = do
  m <- exp
  case m of
    m@(Empty _ _) -> exp
    m@(Inhabitated (q:qs) a) -> \k -> reflect (App m  (Lam $ \ a ->  reify (k a)))

emit :: Exp Int -> [r] -> STM [r] ()
emit a rs _ = shift0 (\c -> do
  as <- c Uni
  pure (Con a as))

emitTwice :: Exp Int -> [r] -> STM [r] ()
emitTwice a rs@(r:s:ss)  =
  bind (emit a rs) (\u -> lift (emit a rs))

reifiedEmitTwice :: Exp (Int -> Stm [List Int,List Int] ())
reifiedEmitTwice = Lam (reify . emitTwice)
