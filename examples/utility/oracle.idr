module Main

import Tezos

%default total

-- A tiny hack for now.
run__IO : a -> a
run__IO f = f

data Storage
  = MkStorage (Map Timestamp Bytes)

data Param : Type where
  Insert : Bytes -> Param
  Check : Timestamp -> Bytes -> List Bytes -> Param

merkleVerify : Bytes -> List Bytes -> Bytes -> Bool
merkleVerify leaf []          root = leaf == root
merkleVerify leaf (Cons x xs) root = merkleVerify (sha256 (leaf <+> x)) xs root

-- Main contract function.
main : (Param, Storage) -> (List Operation, Storage)
main (param, MkStorage map) =
  case param of
    Insert bytes =>
      let timestamp = now in
      if Map.member timestamp map then fail else (Nil, MkStorage (Map.insert timestamp bytes map))
    Check timestamp leaf path =>
      case Map.get timestamp map of
        Nothing   => fail
        Just root =>
          if merkleVerify leaf path root then (Nil, MkStorage map) else fail

-- Basic oracle: periodic hash-commitment to value id, later Merkle tree proof on request of sub-value.
