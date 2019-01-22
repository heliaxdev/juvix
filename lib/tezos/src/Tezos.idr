module Tezos

import public Tezos.Prim

%access public export
%default total

namespace Map

  empty : Map k v
  empty = prim__tezosEmptyMap

  get : k -> Map k v -> Maybe v
  get = prim__tezosMapGet

  update : k -> Maybe v -> Map k v -> Map k v
  update = prim__tezosMapUpdate

  member : k -> Map k v -> Bool
  member = prim__tezosMapMember

  size : Map k v -> Integer
  size = prim__tezosMapSize

amount : Tez
amount = prim__tezosAmount

fail : a
fail = prim__tezosFail

infix 6 ==, /=, <, <=, >, >=
infixl 8 +, -
infixl 9 *, /

interface Eq ty where
  (==) : ty -> ty -> Bool

Eq String where
  (==) = prim__tezosEqString

Eq Integer where
  (==) = prim__tezosEqInt

Eq Tez where
  (==) = prim__tezosEqTez

Eq a => Eq (Maybe a) where
  (==) Nothing  Nothing   = True
  (==) (Just x) (Just y)  = (==) x y
  (==) _        _         = False

interface Num ty where
  (+) : ty -> ty -> ty
  (-) : ty -> ty -> ty
  (*) : ty -> ty -> ty

Num Integer where
  (+) = prim__tezosAddIntInt
  (-) = prim__tezosSubInt
  (*) = prim__tezosMulIntInt

interface Eq ty => Ord ty where
  (<) : ty -> ty -> Bool
  (>) : ty -> ty -> Bool

Ord Integer where
  (<) = prim__tezosLtInt
  (>) = prim__tezosGtInt

Ord Tez where
  (<) = prim__tezosLtTez
  (>) = prim__tezosGtTez

ifThenElse : (b : Bool) -> (t : Lazy a) -> (e : Lazy a) -> a
ifThenElse True  t e = t
ifThenElse False t e = e
