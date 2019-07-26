{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ImpredicativeTypes #-}
import Juvix.Library hiding (foldM, Nat)

-- Attempt 4 is a success!!!

-- Attempt 4 -------------------------------------------------------------------
type AlgebraM (f :: * → *) (x :: *) = forall (r :: *). (r → x) → (f r) → x

type FixM f = forall x. AlgebraM f x → x

foldM :: AlgebraM f x → FixM f → x
foldM alg d = d alg

-- Attempt 4 : Proper mendler style!
data N r = Z | S r deriving (Functor)

type Nat = FixM N

in' :: ∀ (f :: * → *). f (FixM f) → FixM f
in' r f = f (foldM f) r

--out'' :: Functor f ⇒ FixM f → f (FixM f)
out'' rec' fr = fmap (\r → in' (rec' r)) fr --undefined -- fmap (\r → in' (rec' r)) fr

out :: forall x f . Functor f ⇒ (AlgebraM f x → x) → f (AlgebraM f x → x)
out fr = undefined -- fmap (\r → in' r) fr --undefined -- fmap (\r → in' (rec' r)) fr

zero' :: AlgebraM N x -> x
zero' = in' Z

succ' n = in' (S n)

two' :: FixM N
two' = succ' (succ' zero')

three :: FixM N
three = succ' two'

--out' : ∀ F: ★ ➔ ★. Functor ·F ➔ FixM ·F ➔ F ·(FixM ·F)
--  = Λ F. λ fmap. λ d. d ·(F ·(FixM ·F)) (
--    Λ R. λ rec. λ fr. fmap (λ r: R. in (rec r)) fr).

--out' :: forall x f . Functor f ⇒ (AlgebraM f x → x) → f (AlgebraM f x → x)
--out' fr = fr (\ rec' fr → fmap (in' . rec') fr)

--predAlg :: (t -> N (FixM N)) -> N t -> AlgebraM N x -> x
--predAlg :: AlgebraM N (FixM N) -- (t -> N (FixM N)) -> N t -> AlgebraM N x -> x
predAlg :: Nat → Nat
predAlg n =
  case out n of
    Z   → zero'
    S n → n

isEvenAlgN :: AlgebraM N Bool
isEvenAlgN rec' n =
  case n of
    Z   → True
    S n → not (rec' n)

-- attempt 3 : BB encoding/Church with extra! ----------------------------------
type NatBB a = forall b. (() → b) → (a → b) → b
newtype NatBoehm a = Boehm {unBoehm :: NatBB a}

isEvenAlgM :: AlgebraM NatBoehm Bool
isEvenAlgM rec' n = unBoehm n (\_ → True) (\n' →  not (rec' n'))

isEvenM :: FixM NatBoehm → Bool
isEvenM = foldM isEvenAlgM

predMAlg :: p -> NatBoehm (NatBoehm a) -> NatBoehm a
predMAlg _rec' n = unBoehm n (\_ → zero3) (\x → x)

recallNumAlg :: AlgebraM NatBoehm Int
recallNumAlg rec' n = unBoehm n (\_ → 0) (\x → rec' x + 1)

recallNum :: FixM NatBoehm -> Int
recallNum = foldM recallNumAlg

--predM = foldM predMAlg

-- Attempt 3 example
-- infinity type
-- λ alg. (alg (λ f. (f alg)) (λ i. λ j. (i (λ x. x))))
--zero4 = \alg → (alg (\f → (f alg)) (\i → \j → (i (\x → x))))

zero4 :: NatBB a
zero4 = (\ z _s → z ())

zero3 :: NatBoehm a
zero3 = Boehm (\ z _s → z ())

one3 :: NatBoehm (NatBoehm a)
one3 = Boehm (\ _z s → s zero3)

-- attempt 2 : NatF ◂ ★ ➔ ★ = λ R: ★. ∀ X: ★. X ➔ (R ➔ X ➔ X) ➔ X. -------------
type NatF (r :: *) = forall (x :: *). x → (r → x → x) → x
newtype Nat' a = Nat' {unNat :: NatF a}
-- Attempt 2 example
zero2 :: Nat' a
zero2 = Nat' (\z _s → z)

one2 :: Nat' (Nat' a)
one2 = Nat' (\z s → s zero2 z)

-- attempt 1 : Church Encoding -------------------------------------------------
type ChurchNum a = (a -> a) -> a -> a
newtype Church a = Church { unChurch :: ChurchNum a}
