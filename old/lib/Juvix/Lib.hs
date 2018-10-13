{-# LANGUAGE EmptyDataDecls         #-}
{-# LANGUAGE ExplicitForAll         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE UnicodeSyntax          #-}

{-  Prelude-equivalent for Juvix-compiled programs.
    Covers most of the Haskell Prelude API interface with identical semantics (functions with the same name do the same thing).
    This file is mostly a lie; these functions are replaced by the transpiler with Michelson builtins.
    The intent of this library is to be "batteries-included" - so additional common functionality should be added as necessary.
    Subject to API instability until Michelson is finalized.  -}

module Juvix.Lib (

  {- Datatypes -}

  List,
  Set,
  Map,
  Signature,
  Key,
  Hash,
  Contract,
  Account,
  Either(..),
  Maybe(..),
  Pair,
  Unit,
  Tez,
  String,
  Int,
  Nat,
  Bool(..),

  {- General -}

  IO,
  bindIO,
  seqIO,
  returnIO,
  Applicative,
  Functor,
  Monad,
  Monoid,
  pure,
  (<*>),
  fmap,
  (>>=),
  (>>),
  return,
  mappend,
  mempty,
  (<>),

  {- Rebindable Syntax -}

  ifThenElse,
  fromInteger,

  {- Booleans -}

  (&&),
  (||),
  otherwise,
  not,
  xor,

  {- Maybe -}

  fromMaybe,

  {- Comparision -}

  Ord,
  Ordering(..),
  compare,
  compareWithLe,
  compareInt,
  compareNat,
  compareTez,
  max,
  min,
  (<),
  (<=),
  (>),
  (>=),

  leInt,
  leTez,
  leNat,
  leKey,

  {- Equality -}

  Eq,
  (==),
  (/=),
  eqOrdering,
  eqNat,

  {- Arithmetic -}

  Mul,
  Add,
  Div,
  Sub,
  Negate,
  (+),
  (-),
  (*),
  (/),
  negate,
  abs,

  {- Michelson Calls -}

  checkSignature,
  manager,
  defaultAccount,
  source,
  self,
  transferTokens,
  now,
  amount,
  balance,
  hashKey,
  hash,
  stepsToQuota,
  fail,

  {- Misc Michelson -}

  emptySet,
  setMember,
  setUpdate,
  setSize,
  emptyMap,
  mapGet,
  mapMember,
  mapUpdate,
  mapSize,

  mapGetKeyTez,
  mapUpdateKeyTez

) where

{- These are only imported in order to support GHC's literal overloading. -}

import qualified Data.String (String)
import qualified GHC.Num     (Integer)

class Functor f where
  {-# MINIMAL fmap #-}

  fmap ∷ ∀ a b . (a → b) → f a → f b

  (<$) ∷ ∀ a b . a → f b → f a
  (<$) = fmap . const

infixl 4 <$

class (Functor f) ⇒ Applicative f where
  {-# MINIMAL pure, ((<*>) | liftA2) #-}

  pure ∷ ∀ a . a → f a

  (<*>) ∷ ∀ a b . f (a → b) → f a → f b
  (<*>) = liftA2 id

  liftA2 ∷ ∀ a b c . (a → b → c) → f a → f b → f c
  liftA2 f x y = f <$> x <*> y

  (*>) ∷ ∀ a b . f a → f b → f b
  (*>) x y = (id <$ x) <*> y

  (<*) ∷ ∀ a b . f a → f b → f a
  (<*) x y = liftA2 const x y

(<$>) ∷ Functor f ⇒ (a → b) → f a → f b
(<$>) = fmap

infixl 4 <$>

infixl 4 <*>

infixl 4 *>

infixl 4 <*

class (Applicative m) ⇒ Monad m where
  {-# MINIMAL (>>=) #-}

  (>>=) ∷ ∀ a b . m a → (a → m b) → m b

  (>>) ∷ ∀ a b . m a → m b → m b
  (>>) = (*>)
  {-# INLINABLE (>>) #-}

  return ∷ ∀ a . a → m a
  return = pure
  {-# INLINABLE return #-}

infixl 1 >>=

infixl 1 >>

class Eq a where
  {-# MINIMAL (==) | (/=) #-}

  (==) ∷ a → a → Bool
  (==) x y = not (x /= y)
  {-# INLINABLE (==) #-}

  (/=) ∷ a → a → Bool
  (/=) x y = not (x == y)
  {-# INLINABLE (/=) #-}

infix 4 ==

infix 4 /=

class Monoid a where
  {-# MINIMAL mempty, mappend #-}

  mempty ∷ a

  mappend ∷ a → a → a

infixr 6 <>

(<>) ∷ ∀ a . (Monoid a) ⇒ a → a → a
(<>) = mappend

compareWithLe ∷ (Ord a) ⇒ a → a → Ordering
compareWithLe x y =
  if x == y then EQ
    else if x <= y then LT else GT

class (Eq a) ⇒ Ord a where
  {-# MINIMAL compare | (<=) #-}

  compare ∷ a → a → Ordering
  compare = compareWithLe
  {-# INLINABLE compare #-}

  (<) ∷ a → a → Bool
  (<) x y = compare x y == LT
  {-# INLINABLE (<) #-}

  (<=) ∷ a → a → Bool
  (<=) x y = compare x y /= GT
  {-# INLINABLE (<=) #-}

  (>=) ∷ a → a → Bool
  (>=) x y = compare x y /= LT
  {-# INLINABLE (>=) #-}

  (>) ∷ a → a → Bool
  (>) x y = compare x y == GT
  {-# INLINABLE (>) #-}

  max ∷ a → a → a
  max x y = if x >= y then x else y
  {-# INLINABLE max #-}

  min ∷ a → a → a
  min x y = if x <= y then x else y
  {-# INLINABLE min #-}

ifThenElse ∷ ∀ a . Bool → a → a → a
ifThenElse True  t _ = t
ifThenElse False _ f = f

data Ordering
  = LT
  | EQ
  | GT

instance Eq Ordering where
  (==) = eqOrdering

eqOrdering ∷ Ordering → Ordering → Bool
eqOrdering LT LT = True
eqOrdering EQ EQ = True
eqOrdering GT GT = True
eqOrdering _  _  = False

class IsString a where
  fromString ∷ Data.String.String → a

class IsNumber a where
  fromInteger ∷ GHC.Num.Integer → a

class Mul a b c | a b → c where
  (*) ∷ a → b → c

class Add a b c | a b → c where
  (+) ∷ a → b → c

class Sub a b c | a b → c where
  (-) ∷ a → b → c

class Div a b c | a b → c where
  (/) ∷ a → b → c

infixl 7 *

infixl 7 /

infixl 6 +

infixl 6 -

class Negate a where
  negate  ∷ a → a
  abs     ∷ a → a

const ∷ ∀ a b . a → b → a
const x _ = x

id ∷ ∀ a . a → a
id x = x

(.) ∷ ∀ a b c . (b → c) → (a → b) → a → c
(.) f g x = f (g x)

data IO a

instance Functor IO where
  fmap f x = x >>= (return . f)
  {-# INLINABLE fmap #-}

instance Applicative IO where
  pure    = return
  {-# INLINABLE pure #-}


instance Monad IO where
  (>>=)   = bindIO
  {-# NOINLINE (>>=) #-}

  (>>)    = seqIO
  {-# NOINLINE (>>) #-}

  return  = returnIO
  {-# NOINLINE return #-}

bindIO ∷ ∀ a b . IO a → (a → IO b) → IO b
bindIO = rewrite "BindIO"

seqIO ∷ ∀ a b . IO a → IO b → IO b
seqIO = rewrite "SeqIO"

returnIO ∷ ∀ a . a → IO a
returnIO = rewrite "ReturnIO"

type List a = [a]

{-  May make sense to encapsulate common functionality between maps/sets in typeclasses. -}

data Set a

emptySet ∷ ∀ a . Set a
emptySet = rewrite "EmptySetUT"

setMember ∷ ∀ a . a → Set a → Bool
setMember = rewrite "SetMemUT"

setUpdate ∷ ∀ a . a → Bool → Set a → Set a
setUpdate = rewrite "SetUpdateUT"

setSize ∷ ∀ a . Set a → Int
setSize = rewrite "SetSizeUT"

data Map k v

emptyMap ∷ ∀ k v . (Ord k) ⇒ Map k v
emptyMap = rewrite "EmptyMapUT"

mapGet ∷ ∀ k v . (Ord k) ⇒ k → Map k v → Maybe v
mapGet = rewrite "MapGetUT"

mapGetKeyTez ∷ Key → Map Key Tez → Maybe Tez
mapGetKeyTez = rewrite "MapGetUT"

mapMember ∷ ∀ k v . (Ord k) ⇒ k → Map k v → Bool
mapMember = rewrite "MapMemberUT"

mapUpdate ∷ ∀ k v . (Ord k) ⇒ k → Maybe v → Map k v → Map k v
mapUpdate = rewrite "MapUpdateUT"

mapUpdateKeyTez ∷ Key → Maybe Tez → Map Key Tez → Map Key Tez
mapUpdateKeyTez = rewrite "MapUpdateUT"

mapSize ∷ ∀ k v . (Ord k) ⇒ Map k v → Int
mapSize = rewrite "MapSizeUT"

data Signature

checkSignature ∷ ∀ a . Key → (Signature, a) → Bool
checkSignature = rewrite "CheckSignatureUT"

data Key

data Hash

instance Eq Key

instance Ord Key

leKey ∷ Key → Key → Bool
leKey _ _ = rewrite "FailUT"

data Contract a b

type Account = Contract () ()

manager ∷ ∀ a b . Contract a b → Key
manager = rewrite "ManagerUT"

defaultAccount ∷ Hash → Account
defaultAccount = rewrite "DefaultAccountUT"

source ∷ ∀ a b . Contract a b
source = rewrite "SourceUT"

self ∷ ∀ a b . Contract a b
self = rewrite "SelfUT"

{-  c *must* be the type of the contract's storage - at the moment this is not enfored by the Haskell typesystem  -}

transferTokens ∷ ∀ a b c . a → Tez → Contract a b → c → IO (b, c)
transferTokens = rewrite "TransferTokensUT"

data Either a b
  = Left a
  | Right b

data Maybe a
  = Just a
  | Nothing

fromMaybe ∷ ∀ a . a → Maybe a → a
fromMaybe _ (Just v) = v
fromMaybe d Nothing  = d

type Pair a b = (a, b)

type Unit = ()

unit ∷ Unit
unit = ()

data Timestamp

now ∷ Timestamp
now = rewrite "NowUT"

data Nat

data Tez

amount ∷ Tez
amount = rewrite "AmountUT"

balance ∷ Tez
balance = rewrite "BalanceUT"

data Bool
  = True
  | False

otherwise ∷ Bool
otherwise = True

infixr 3 &&

(&&) ∷ Bool → Bool → Bool
(&&) = rewrite "AndUT"

infixr 2 ||

(||) ∷ Bool → Bool → Bool
(||) = rewrite "OrUT"

xor ∷ Bool → Bool → Bool
xor = rewrite "XorUT"

not ∷ Bool → Bool
not = rewrite "NotUT"

data String

hashKey ∷ Key → Hash
hashKey = rewrite "HashKeyUT"

hash ∷ a → String
hash = rewrite "HUT"

data Int

{-  This is *not* a side effect - however, it makes no sense without specified evaluation order, so it's in the IO monad.
    Not sure if this is useful with Juvix anyways; if the quota is static the transpiler can analyze it.  -}

stepsToQuota ∷ IO Int
stepsToQuota = rewrite "StepsToQuotaUT"

{-  Typeclass Instances   -}

{-  Equality      -}

instance Eq Tez where
  x == y = (rewrite "EqUT") (x - y)
  {-# INLINEABLE (==) #-}

instance Eq Int where
  x == y = (rewrite "EqUT") (x - y)
  {-# INLINEABLE (==) #-}

instance Eq Nat where
  x == y = eqNat x y
  {-# INLINEABLE (==) #-}

eqNat ∷ Nat → Nat → Bool
eqNat x y = (rewrite "EqUT") (x - y)

{-  Comparision   -}

instance Ord Tez where
  (<=) = leTez
  {-# INLINEABLE (<=) #-}

leTez ∷ Tez → Tez → Bool
leTez x y = (rewrite "LeUT") (x - y)

compareTez ∷ Tez → Tez → Ordering
compareTez = compareWithLe

instance Ord Int where
  (<=) = leInt
  {-# INLINEABLE (<=) #-}

leInt ∷ Int → Int → Bool
leInt x y = (rewrite "LeUT") (x - y)

compareInt ∷ Int → Int → Ordering
compareInt = compareWithLe

instance Ord Nat where
  (<=) = leNat
  {-# INLINEABLE (<=) #-}

leNat ∷ Nat → Nat → Bool
leNat x y = (rewrite "LeUT") (x - y)

compareNat ∷ Nat → Nat → Ordering
compareNat = compareWithLe

{-  Arithmetic    -}

instance Add Int Int Int where
  (+) = rewrite "AddIntIntUT"

instance Sub Int Int Int where
  (-) = rewrite "SubIntUT"

instance Mul Int Int Int where
  (*) = rewrite "MulIntIntUT"

instance Div Int Int Int where
  (/) = rewrite "EdivIntIntUT"

instance Negate Int where
  negate  = rewrite "NegIntUT"
  abs     = rewrite "AbsIntUT"

instance Negate Nat where
  negate  = rewrite "NegNatUT"
  abs     = rewrite "AbsNatUT"

instance Add Nat Nat Nat where
  (+) = rewrite "AddNatNatUT"

instance Sub Nat Nat Nat where
  (-) = rewrite "SubNatNatUT"

instance Mul Nat Nat Nat where
  (*) = rewrite "MulNatNatUT"

instance Div Nat Nat Nat where
  (/) = rewrite "EdivNatNatUT"

instance Add Tez Tez Tez where
  (+) = rewrite "AddTezUT"

instance Sub Tez Tez Tez where
  (-) = rewrite "SubTezUT"

instance Add Int Nat Int where
  (+) = rewrite "AddIntNatUT"

instance Add Nat Int Int where
  (+) = rewrite "AddNatIntUT"

instance Mul Int Nat Int where
  (*) = rewrite "MulIntNatUT"

instance Mul Nat Int Int where
  (*) = rewrite "MulNatIntUT"

{-  Strings   -}

instance Monoid String where
  mempty  = fromString ""
  mappend = rewrite "ConcatUT"

{-  For string literal overloading.   -}

instance IsString String where
  fromString = rewrite "stringFromString"

{-  Never exposed; we need these for numeric literal overloading.   -}

instance IsNumber Tez where
  fromInteger = rewrite "tezFromInteger"
  {-# NOINLINE fromInteger #-}

instance IsNumber Int where
  fromInteger = rewrite "intFromInteger"
  {-# NOINLINE fromInteger #-}

instance IsNumber Nat where
  fromInteger = rewrite "natFromInteger"
  {-# NOINLINE fromInteger #-}

fail ∷ IO a
fail = rewrite "FailUT"

{-  Builtin library calls, rewritten by Juvix.  -}

rewrite ∷ ∀ a b . a → b
rewrite label = let x = x in x
{-# NOINLINE rewrite #-}
