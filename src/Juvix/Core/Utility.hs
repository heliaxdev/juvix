module Juvix.Core.Utility where

import Juvix.Library
import Prelude ((!!))

unDeBruijin ∷
  ( HasState "nextName" Int m,
    HasState "nameStack" [Int] m
  ) ⇒
  Int →
  m Symbol
unDeBruijin ind = do
  stack ← get @"nameStack"
  pure (intern $ show $ stack !! ind)

newName ∷
  ( HasState "nextName" Int m,
    HasState "nameStack" [Int] m
  ) ⇒
  m Symbol
newName = do
  name ← get @"nextName"
  modify @"nextName" (+ 1)
  modify @"nameStack" ((:) name)
  return (intern (show name))
