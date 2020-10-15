module Juvix.EffectHandlers.CPS.Continuation where

newtype Cps r a = Cps { unCps :: (a -> r) -> r}

-- Stm transforms (Cps (Cps (Cps d) c) b) a
-- into Stm [b c d] a
-- Stm also talks about
newtype Stm (rs :: * -> *) a = Stm (Cps (Stm rs r) a)

-- Any expression language of kind * -> *
-- with lambda abstraction and application
data Exp :: * -> * where
  Lam :: (Exp a -> Exp b) -> Exp (a -> b)
  App :: Exp (a -> b) -> Exp a -> Exp b

data Statement (rs :: * -> *) a where
  Value :: a -> Statement [r] (Exp a)
  Computation :: Context [r] a r -> Statement [r] r

data Context :: (* -> *) -> * -> * -> * where
  Static  :: (Exp a -> Statement rs b)  -> Context rs a b
  Dynamic ::  Exp (a -> Stm rs b) -> Context rs a b

reify :: Statement rs a -> Exp (Stm rs a)
reify m@(Value [] _) = m
reify m@(Computation _) = Lam $ \ k ->  reify (m (Dynamic k))

reflect :: Exp (Stm rs a) -> Statement rs a
reflect m = \k -> reflect (App m  (reifyContext k))

reifyContext :: Context rs a r -> Exp (a -> Stm rs r)
reifyContext (Static k) = Lam $ \ a ->  reify (k a)
reifyContext (Dynamic k) = k

resume :: Context rs a r -> (Exp a -> Statement rs r)
resume (Static k)  = k
resume (Dynamic k) = \a -> reflect (App k  a)

pure :: Exp a -> Statement rs a
pure = Value

resumeWithPureValue :: Exp a -> Context rs r a -> Statement rs a
resumeWithPureValue a = \k -> resume k a

push :: Context rs a b -> Context rs b r -> Context rs a r
push f k = Static (\a -> resume f a k)

bind :: Statement rs a -> Context rs a b -> Statement rs b
bind m@(Value [] _)    f = resume f m
bind m@(Computation _) f = \k -> m (push f k)

shift0 :: (Context rs a r -> Statement rs r) -> Statement rs a
shift0 = id

runIn0 :: Statement rs a -> Context rs a r -> Statement rs r
runIn0 = id

reset0 :: Statement rs a -> Statement rs a
reset0 m = runIn0 m (Static pure)
