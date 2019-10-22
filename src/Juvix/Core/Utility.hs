module Juvix.Core.Utility where

import Data.List (findIndex)
import Juvix.Library
import Prelude ((!!))

pushName ∷
  (HasState "symbolStack" [Symbol] m) ⇒
  Symbol →
  m ()
pushName name = modify @"symbolStack" ((:) name)

lookupName ∷
  (HasState "symbolStack" [Symbol] m) ⇒
  Symbol →
  m (Maybe Int)
lookupName name = do
  stack ← get @"symbolStack"
  let ind = findIndex ((==) name) stack
  pure ind

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
