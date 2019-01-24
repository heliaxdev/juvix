module Tezos

import public Interfaces
import public Basics

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

  foldl : (a -> v -> a) -> a -> Map k v -> a
  foldl = prim__tezosFail

namespace Set

  empty : Set v
  empty = prim__tezosEmptySet

  member : v -> Set v -> Bool
  member = prim__tezosSetMember

  size : Set v -> Integer
  size = prim__tezosSetSize

amount : Tez
amount = prim__tezosAmount

fail : a
fail = prim__tezosFail

sender : Address
sender = prim__tezosSender

transferTokens : p -> Tez -> Contract p -> Operation
transferTokens = prim__tezosTransferTokens

setDelegate : Maybe KeyHash -> Operation
setDelegate = prim__tezosSetDelegate

checkSignature : Key -> Signature -> Bytes -> Bool
checkSignature = prim__tezosCheckSignature

Eq String where
  (==) = prim__tezosEqString

Eq Integer where
  (==) = prim__tezosEqInt

Eq Nat where
  (==) = prim__tezosEqNat

Eq Tez where
  (==) = prim__tezosEqTez

Eq Address where
  (==) = prim__tezosEqAddress

Eq a => Eq (Maybe a) where
  (==) Nothing  Nothing   = True
  (==) (Just x) (Just y)  = (==) x y
  (==) _        _         = False

Num Integer where
  (+) = prim__tezosAddIntInt
  (*) = prim__tezosMulIntInt
  fromInteger = id

Num Nat where
  (+) = prim__tezosAddNatNat
  (*) = prim__tezosMulNatNat
  fromInteger = prim__tezosFail

Neg Nat where
  negate = prim__tezosFail
  (-) = prim__tezosSubNat

Ord Integer where
  (<) = prim__tezosLtInt
  (>) = prim__tezosGtInt

Ord Nat where
  (<) = prim__tezosLtNat
  (>) = prim__tezosGtNat

Ord Tez where
  (<) = prim__tezosLtTez
  (>) = prim__tezosGtTez

ifThenElse : (b : Bool) -> (t : Lazy a) -> (e : Lazy a) -> a
ifThenElse True  t e = t
ifThenElse False t e = e
