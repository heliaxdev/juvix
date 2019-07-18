{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}

module Juvix.Backends.Env where

import Juvix.Library
import Juvix.Backends.Interface

-- | Info Stores diagnostic data on how much memory a particular graph reduction uses
data Info = Info { memoryAllocated  :: Integer
                 , sequentalSteps   :: Integer
                 , parallelSteps    :: Integer
                 , biggestGraphSize :: Integer
                 , currentGraphSize :: Integer
                 } deriving (Show)

data InfoNet net = InfoNet {net :: net, info :: Info} deriving (Show, Generic)

type InfoNetwork     net a m = (HasState "info" Info m
                               , HasState "net" (net a) m
                               , Network net)
type InfoNetworkDiff net a m = (HasState "info" Info m
                               , HasState "net" (net a) m
                               , DifferentRep net)

newtype EnvNetInfo net a = EnvI (State (InfoNet net) a)
  deriving (Functor, Applicative, Monad)
  deriving (HasState "info" Info) via
    Field "info" () (MonadState (State (InfoNet net)))
  deriving (HasState "net" net) via
    Field "net" () (MonadState (State (InfoNet net)))

runInfoNet :: EnvNetInfo net a → InfoNet net → (InfoNet net)
runInfoNet (EnvI m) = execState m

--runNet :: EnvNetInfo b a → net → InfoNet b
runNet :: EnvNetInfo net a → net → Integer → InfoNet net
runNet f net size = runInfoNet f (InfoNet net (Info size 0 0 size  size))

-- sequentalStep ∷ MonadState StateInfo m ⇒ m ()
sequentalStep :: HasState "info" Info m ⇒ m ()
sequentalStep = modify' @"info" (\c → c {sequentalSteps = sequentalSteps c + 1})

-- incGraphSizeStep :: MonadState StateInfo m ⇒ Integer → m ()
incGraphSizeStep :: HasState "info" Info m ⇒ Integer → m ()
incGraphSizeStep n = do
  Info memAlloced seqStep parallelSteps largestGraph currGraph ← get @"info"
  let memoryAllocated | n > 0 = memAlloced + n
                      | otherwise = memAlloced
      currentGraphSize = n + currGraph
      biggestGraphSize = max currentGraphSize largestGraph
      sequentalSteps = succ seqStep
  put @"info" Info { memoryAllocated, currentGraphSize
                   , biggestGraphSize, sequentalSteps, parallelSteps }
