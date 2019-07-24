import Juvix.Library hiding (foldM)

-- attempt 1 : Church Encoding
type ChurchNum a = (a -> a) -> a -> a
newtype Church a = Church { unChurch :: ChurchNum a}

-- attempt 2 : NatF ◂ ★ ➔ ★ = λ R: ★. ∀ X: ★. X ➔ (R ➔ X ➔ X) ➔ X.
type NatF (r :: *) = forall (x :: *). x → (r → x → x) → x
newtype Nat' a = Nat' {unNat :: NatF a}

-- attempt 3 : BB encoding/Church with extra!
type NatBB a = forall b. (() → b) → (a → b) → b
newtype NatBoehm a = Boehm {unBoehm :: NatBB a}

type AlgebraM (f :: * → *) (x :: *) = forall (r :: *). (r → x) → (f r) → x

isEvenAlgM :: AlgebraM NatBoehm Bool
isEvenAlgM rec' n = unBoehm n (\_ → True) (\n' →  not (rec' n'))

type FixM f = forall x. AlgebraM f x → x

foldM :: AlgebraM f x → FixM f → x
foldM alg d = d alg

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

-- Attempt 2 example
zero2 :: Nat' a
zero2 = Nat' (\z _s → z)

one2 :: Nat' (Nat' a)
one2 = Nat' (\z s → s zero2 z)
