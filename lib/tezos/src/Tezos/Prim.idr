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

data Either : (l : Type, r : Type) -> Type

data Map : (k : Type, v : Type) -> Type

data List : (e : Type) -> Type

data Maybe : (v : Type) -> Type where
  Nothing : Maybe v
  Just : (x : v) -> Maybe v

-- Primitives

%extern prim__tezosAmount : Integer

%extern prim__tezosNil : List a

%extern prim__tezosMapEmpty : Map k v

%extern prim__tezosMapGet : k -> Map k v -> Maybe v

%extern prim__tezosMapMember : k -> Map k v -> Bool

%extern prim__tezosMapUpdate : k -> Maybe v -> Map k v -> Map k v

%extern prim__tezosMapSize : Map k v -> Integer

%extern prim__tezosConsPair : (x : a) -> (y : b) -> Pair a b

%extern prim__tezosAddIntInt : Integer -> Integer -> Integer

%extern prim__tezosSubIntInt : Integer -> Integer -> Integer

%extern prim__tezosMulIntInt : Integer -> Integer -> Integer

%extern prim__tezosEqString : String -> String -> Bool

-- Syntax rules
