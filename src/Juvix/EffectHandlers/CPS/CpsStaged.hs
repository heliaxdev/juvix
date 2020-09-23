module Juvix.EffectHandlers.CPS.CpsStaged where

import Juvix.EffectHandlers.CPS.CpsUnstaged

data Exp :: * -> * where
  Lam :: (Exp a -> Exp b) -> Exp (a -> b)
  App :: Exp (a -> b) -> Exp a -> Exp b
  Var :: String -> Exp a
  Tru :: Exp Bool
  Fls :: Exp Bool
  Uni :: Exp ()
  Lit :: Int -> Exp Int
  Add :: Exp Int -> Exp Int -> Exp Int
  Sub :: Exp Int -> Exp Int -> Exp Int
  Sma :: Exp Int -> Exp Int -> Exp Bool
  Equ :: Exp Int -> Exp Int -> Exp Bool
  Tri :: Exp Int -> Exp Int -> Exp Int -> Exp (Int, Int, Int)
  Dis :: Exp String -> Exp (IO ())
  Emp :: Exp (List a)
  Con :: Exp a -> Exp (List a) -> Exp (List a)
  Str :: String -> Exp String
  Cat :: Exp String -> Exp String -> Exp String
  Noh :: Exp (Maybe a)
  Jus :: Exp a -> Exp (Maybe a)
  Ski :: Exp (IO ())
  Seq :: Exp (IO ()) -> Exp (IO ()) -> Exp (IO ())
  Ite :: Exp Bool -> Exp a -> Exp a -> Exp a
  Rec :: (Exp (a -> b) -> Exp a -> Exp b) -> Exp (a -> b)

pretty :: Int -> Exp a -> String
pretty s (Lam f) = "(\\x" ++ show s ++ " => " ++ pretty (s + 1) (f (Var ("x" ++ show s))) ++ ")"
pretty s (App f x) = "(" ++ pretty s f ++ " " ++ pretty s x ++ ")"
pretty s (Var x) = x
pretty s Tru = "True"
pretty s Fls = "False"
pretty s Uni = "()"
pretty s (Lit n) = show n
pretty s (Add l r) = "(" ++ pretty s l ++ " + " ++ pretty s r ++ ")"
pretty s (Sub l r) = "(" ++ pretty s l ++ " - " ++ pretty s r ++ ")"
pretty s (Sma l r) = "(" ++ pretty s l ++ " < " ++ pretty s r ++ ")"
pretty s (Equ l r) = "(" ++ pretty s l ++ " == " ++ pretty s r ++ ")"
pretty s (Tri i j k) = "(" ++ pretty s i ++ ", " ++ pretty s j ++ ", " ++ pretty s k ++ ")"
pretty s (Dis ijk) = "(putStrLn " ++ pretty s ijk ++ ")"
pretty s Emp = "[]"
pretty s (Con h t) = "(" ++ pretty s h ++ " ::: " ++ pretty s t ++ ")"
pretty s (Str a) = "\"" ++ a ++ "\""
pretty s (Cat l r) = "(" ++ pretty s l ++ " ++ " ++ pretty s r ++ ")"
pretty s Noh = "Nothing"
pretty s (Jus a) = "(Just " ++ pretty s a ++ ")"
pretty s Ski = "(return ())"
pretty s (Seq l r) = "(" ++ pretty s l ++ " >> " ++ pretty s r ++ ")"
pretty s (Ite b t e) = "(if " ++ pretty s b ++ " then " ++ pretty s t ++ " else " ++ pretty s e ++ ")"
pretty s (Rec f) = "(let f" ++ show s ++ " x" ++ show (s + 1) ++ " = " ++ pretty (s + 2) (f (Var ("f" ++ show s)) (Var ("x" ++ show (s + 1)))) ++ " in f" ++ show s ++ ")"

newtype CPS r a = CPS { unCPS :: (Exp a -> Exp r) -> Exp r}

shift0 :: ((Exp a -> Exp r) -> Exp r) -> CPS r a
shift0 = id

runIn0 :: CPS r a -> (Exp a -> Exp r) -> Exp r
runIn0 = id

reset0 :: CPS a a -> Exp a
reset0 m = runIn0 m id

pure :: Exp a -> CPS r a
pure a = \k -> k a

push :: (Exp a -> CPS r b) -> (Exp b -> Exp r) -> (Exp a -> Exp r)
push f k = \a -> f a k

bind :: CPS r a -> (Exp a -> CPS r b) -> CPS r b
bind m f = \k -> m (push f k)

(>>=) :: CPS r a -> (Exp a -> CPS r b) -> CPS r b
(>>=) = bind

reify :: CPS r a -> Exp (Cps r a)
reify m    = (Lam $ \ k ->  m (\a -> App k  a))

reflect :: Exp (Cps r a) -> CPS r a
reflect m  = \k -> App m  (Lam $ \ a ->  k a)

app :: CPS r (a -> Cps r b) -> CPS r a -> CPS r b
app mf ma = do
  f <- mf
  a <- ma
  reflect (App f  a)

lam :: (Exp a -> CPS r b) -> CPS r (a -> Cps r b)
lam f = pure (Lam $ \ a ->  reify (f a))

export
example :: Exp Int
example = Add (Lit 1) (reset0 (do
  x <- shift0 (\c -> c (c (Lit 100)))
  pure (Add (Lit 10) x)))

resumeTwice :: CPS Int (Int -> Cps Int Int)
resumeTwice = lam (\n -> shift0 (\c -> c (c n)))

example' :: Exp Int
example' = Add (Lit 1) (reset0 (do
  x <- app resumeTwice (pure (Lit 100))
  pure (Add (Lit 10) x)))
