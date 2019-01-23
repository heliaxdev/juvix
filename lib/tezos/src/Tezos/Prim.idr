module Tezos.Prim

%access public export
%default total

-- Datatypes

data Bool = False | True

data Key

data Hash

data Integer

data Nat

data Timestamp

data Tez

data String

data Operation

data Either : (l : Type, r : Type) -> Type where
  Left  : l -> Either l r
  Right : r -> Either l r

data Map : (k : Type, v : Type) -> Type

data BigMap : (k : Type, v : Type) -> Type

data Set : (v : Type) -> Type

data List : (e : Type) -> Type where
  Nil   : List e
  Cons  : e -> List e -> List e

data Maybe : (v : Type) -> Type where
  Nothing : Maybe v
  Just    : (x : v) -> Maybe v

-- Primitives
-- In order of Michelson spec listing.

%extern prim__tezosConsPair : (x : a) -> (y : b) -> Pair a b

%extern prim__tezosListMap : (f : a -> b) -> List a -> List b

%extern prim__tezosListReduce : ()

%extern prim__tezosEmptySet : Set a

%extern prim__tezosSetMap : (f : a -> b) -> Set a -> Set b

%extern prim__tezosEmptyMap : Map k v

%extern prim__tezosMapGet : k -> Map k v -> Maybe v

%extern prim__tezosMapMember : k -> Map k v -> Bool

%extern prim__tezosMapUpdate : k -> Maybe v -> Map k v -> Map k v

%extern prim__tezosMapSize : Map k v -> Integer

%extern prim__tezosAddIntInt : Integer -> Integer -> Integer

%extern prim__tezosSubInt : Integer -> Integer -> Integer

%extern prim__tezosMulIntInt : Integer -> Integer -> Integer

%extern prim__tezosAddNatNat : Nat -> Nat -> Nat

%extern prim__tezosSubNat : Nat -> Nat -> Nat

%extern prim__tezosMulNatNat : Nat -> Nat -> Nat

%extern prim__tezosEqNat : Nat -> Nat -> Bool

%extern prim__tezosLtNat : Nat -> Nat -> Bool

%extern prim__tezosGtNat : Nat -> Nat -> Bool

%extern prim__tezosEqInt : Integer -> Integer -> Bool

%extern prim__tezosLtInt : Integer -> Integer -> Bool

%extern prim__tezosGtInt : Integer -> Integer -> Bool

%extern prim__tezosEqTez : Tez -> Tez -> Bool

%extern prim__tezosLtTez : Tez -> Tez -> Bool

%extern prim__tezosGtTez : Tez -> Tez -> Bool

%extern prim__tezosEqString : String -> String -> Bool

%extern prim__tezosFail : a

%extern prim__tezosManager : Key

%extern prim__tezosNow : Timestamp

%extern prim__tezosBalance : Tez

%extern prim__tezosAmount : Tez

-- Syntax rules
