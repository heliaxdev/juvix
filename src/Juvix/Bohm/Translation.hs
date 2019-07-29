{-# LANGUAGE NamedFieldPuns #-}

module Juvix.Bohm.Translation(astToNet, netToAst) where

import qualified Data.Map.Strict      as Map

import           Juvix.Library        hiding (link, empty)
import           Juvix.Backends.Graph
import           Juvix.Backends.Interface
import qualified Juvix.Nets.Bohm      as B
import qualified Juvix.Bohm.Type      as BT

data Env net = Env {level :: Int
                   , net' :: net B.Lang
                   , free :: Map SomeSymbol (Node, PortType)
                   } deriving (Generic)

newtype EnvState net a = EnvS (State (Env net) a)
  deriving (Functor, Applicative, Monad)
  deriving (HasState "level" Int) via
    Rename "level" (Field "level" () (MonadState (State (Env net))))
  deriving (HasState "free" (Map SomeSymbol (Node, PortType))) via
    (Field "free" () (MonadState (State (Env net))))
  deriving (HasState "net" (net B.Lang)) via
    Rename "net'" (Field "net'" () (MonadState (State (Env net))))

execEnvState :: Network net ⇒ EnvState net a → Env net → Env net
execEnvState (EnvS m) = execState m

astToNet :: Network net ⇒ BT.Bohm → net B.Lang
astToNet bohm = net'
  where
    Env {net'} = execEnvState (recursive bohm Map.empty) (Env 0 empty mempty)
    -- we return the port which the node above it in the AST connects to!
    recursive (BT.IntLit x) _context = (,) <$> newNode (B.Primar $ B.IntLit x) <*> pure Prim
    recursive BT.False'     _context = (,) <$> newNode (B.Primar B.Fals)       <*> pure Prim
    recursive BT.True'      _context = (,) <$> newNode (B.Primar B.Tru)        <*> pure Prim
    recursive BT.Nil        _context = (,) <$> newNode (B.Primar B.Nil)        <*> pure Prim
    recursive (BT.Car b)     context = genericAux1PrimArg b (B.Auxiliary1 B.Car)     context
    recursive (BT.Cdr b)     context = genericAux1PrimArg b (B.Auxiliary1 B.Cdr)     context
    recursive (BT.IsNil b)   context = genericAux1PrimArg b (B.Auxiliary1 B.TestNil) context
    recursive (BT.Not b)     context = genericAux1PrimArg b (B.Auxiliary1 B.Not)     context
    recursive (BT.Infix' BT.Mult b1 b2) c     = genericAux2PrimArg b1 b2 (B.Auxiliary2 $ B.Infix B.Prod)  c
    recursive (BT.Infix' BT.Plus b1 b2) c     = genericAux2PrimArg b1 b2 (B.Auxiliary2 $ B.Infix B.Add)   c
    recursive (BT.Infix' BT.Sub b1 b2)  c     = genericAux2PrimArg b1 b2 (B.Auxiliary2 $ B.Infix B.Sub)   c
    recursive (BT.Infix' BT.Mod b1 b2)  c     = genericAux2PrimArg b1 b2 (B.Auxiliary2 $ B.Infix B.Mod)   c
    recursive (BT.Infix' BT.Eq b1 b2)   c     = genericAux2PrimArg b1 b2 (B.Auxiliary2 $ B.InfixB B.Eq)   c
    recursive (BT.Infix' BT.Neq b1 b2)  c     = genericAux2PrimArg b1 b2 (B.Auxiliary2 $ B.InfixB B.Neq)  c
    recursive (BT.Infix' BT.Lt b1 b2)   c     = genericAux2PrimArg b1 b2 (B.Auxiliary2 $ B.InfixB B.Less) c
    recursive (BT.Infix' BT.Gt b1 b2)   c     = genericAux2PrimArg b1 b2 (B.Auxiliary2 $ B.InfixB B.More) c
    recursive (BT.Infix' BT.Ge b1 b2)   c     = genericAux2PrimArg b1 b2 (B.Auxiliary2 $ B.InfixB B.Meq)  c
    recursive (BT.Infix' BT.Le b1 b2)   c     = genericAux2PrimArg b1 b2 (B.Auxiliary2 $ B.InfixB B.Leq)  c
    recursive (BT.Infix' BT.Division b1 b2) c = genericAux2PrimArg b1 b2 (B.Auxiliary2 $ B.Infix B.Div)   c
    recursive (BT.Application b1 b2)    c     = genericAux2PrimArg b1 b2 (B.Auxiliary2 B.App) c
    recursive (BT.Infix' BT.Or b1 b2)   c     = genericAux2PrimArg b1 b2 (B.Auxiliary2 B.Or)  c
    recursive (BT.Infix' BT.And b1 b2)  c     = genericAux2PrimArg b1 b2 (B.Auxiliary2 B.And) c
    recursive (BT.Cons b1 b2)           c     = genericAux2 (b1, Aux2) (b2, Aux1) (B.Auxiliary2 B.Cons, Prim) c
    recursive (BT.Symbol' s) context = do
      frees ← get @"free"
      case (context Map.!? s, frees Map.!? s) of
        -- The symbol is bound, and thus we have a port and its number
        (Just portInfo, _) → chaseAndCreateFan portInfo
        -- The symbol is Free, but used already, create a sharing node for it
        (Nothing, Just portInfo) → chaseAndCreateFan portInfo
        -- The symbol is Free, just stash it in a symbol with no rewrite rules
        (Nothing, Nothing) → do
          nodeInfo ← (,) <$> (newNode (B.Primar $ B.Symbol s)) <*> pure Prim
          put @"free" (Map.insert s nodeInfo frees)
          pure nodeInfo
    recursive (BT.Lambda s body) context = do
      numLam          ← newNode (B.Auxiliary2 B.Lambda)
      (bNode,  bPort) ← recursive body (Map.insert s (numLam, Aux2) context)
      link (numLam, Aux1) (bNode, bPort)
      aux2Filled ← findEdge (numLam, Aux2)
      case aux2Filled of
        Nothing → do
          numErase ← newNode (B.Primar B.Erase)
          link (numLam, Aux2) (numErase, Prim)
        Just _  → pure ()
      pure (numLam, Prim)
    recursive (BT.Let sym bound body) context = do
      (numBound, portBound) ← recursive bound context
      recursive body (Map.insert sym (numBound, portBound) context)
    recursive (BT.Letrec sym body) context = do
      numMu          ← newNode (B.Auxiliary2 B.Mu)
      (bNode, bPort) ← recursive body (Map.insert sym (numMu, Aux2) context)
      link (numMu, Aux1) (bNode, bPort)
      pure (numMu, Prim)
    recursive (BT.If b1 b2 b3) c = do
      (numIf, retPort) ← genericAux2 (b1, Prim) (b2, Aux3) (B.Auxiliary3 B.IfElse, Aux1) c
      (b3Num, b3Port)  ← recursive b3 c
      link (numIf, Aux2) (b3Num, b3Port)
      pure (numIf, retPort)
    genericAux1 (b1, pb1) (langToCreate, portToReturn) context = do
      numCar        ← newNode langToCreate
      (bNum, bPort) ← recursive b1 context
      link (bNum, bPort) (numCar, pb1)
      pure (numCar, portToReturn)
    genericAux2 (b1, pb1) (b2, pb2) retInfo context = do
      (numApp, retPort) ← genericAux1 (b1, pb1) retInfo context
      (b2Num, b2Port)   ← recursive b2 context
      link (numApp, pb2) (b2Num, b2Port)
      pure (numApp, retPort)
    genericAux2PrimArg b1 b2 lc        = genericAux2 (b1, Prim) (b2, Aux2) (lc, Aux1)
    genericAux1PrimArg b1 langToCreate = genericAux1 (b1, Prim) (langToCreate, Aux1)

netToAst :: Net B.Lang → BT.Bohm
netToAst = undefined

-- Helper Functions-------------------------------------------------------------

-- | Creates a fan if the port is taken, and prepares to be connected
chaseAndCreateFan :: (Network net, HasState "level" Int m, HasState "net" (net B.Lang) m)
                  ⇒ (Node, PortType)
                  → m (Node, PortType)
chaseAndCreateFan (num,port) = do
  lev  ← get @"level"
  edge ← findEdge (num, port)
  case edge of
    Nothing → pure (num, port)
    Just t1@(nConnected, connectedPort) → do
      put @"level" (succ lev)
      numFan ← newNode (B.Auxiliary2 $ B.FanIn lev)
      let nodeFan = RELAuxiliary2 { node       = numFan
                                  , primary    = Link (Port port          num)
                                  , auxiliary1 = Link (Port connectedPort nConnected)
                                  , auxiliary2 = Link FreePort
                                  }
      linkAll nodeFan
      deleteEdge t1 (num,port)
      pure (numFan, Aux2)
