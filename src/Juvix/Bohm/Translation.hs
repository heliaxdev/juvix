{-# LANGUAGE TemplateHaskell #-}

module Juvix.Bohm.Translation(astToNet, netToAst) where

import           Protolude hiding (link)
import qualified Data.Map.Strict as Map
-- eventually abstract out this import
import qualified Data.Graph.Inductive as G

import           Juvix.Interaction
import qualified Juvix.Nets.Bohm   as B
import qualified Juvix.Bohm.Type   as BT
import           Control.Lens

data Env = Env {_level :: Int, _net :: Net B.Lang}
         deriving (Show)

makeLenses ''Env

newNodeM :: MonadState Env m => B.Lang -> m Node
newNodeM term = do
  env ← get
  let (numLam, net') = newNode (env^.net) term
  put (set net net' env)
  pure numLam


linkM :: MonadState Env m ⇒ (Node, PortType) → (Node, PortType) → m ()
linkM node1I node2I =
  modify (over net (\n → link n node1I node2I))

astToNet :: BT.Bohm → Net B.Lang
astToNet bohm = finalEnv^.net
  where
    finalEnv = execState (recursive bohm Map.empty) (Env 0 G.empty)
    -- we return the port which the node above it in the AST connects to!
    recursive (BT.IntLit x) _context = (,) <$> newNodeM (B.IntLit' x) <*> pure Prim
    recursive BT.False'     _context = (,) <$> newNodeM B.Fals'       <*> pure Prim
    recursive BT.True'      _context = (,) <$> newNodeM B.Tru'        <*> pure Prim
    recursive BT.Nil        _context = (,) <$> newNodeM B.Nil'        <*> pure Prim
    recursive (BT.Car b)     context = genericAux1PrimArg b B.Car'     context
    recursive (BT.Cdr b)     context = genericAux1PrimArg b B.Cdr'     context
    recursive (BT.IsNil b)   context = genericAux1PrimArg b B.TestNil' context
    recursive (BT.Not b)     context = genericAux1PrimArg b B.Not'     context
    recursive (BT.Application b1 b2)    c = genericAux2PrimArg b1 b2 B.App'  c
    recursive (BT.Infix' BT.Mult b1 b2) c = genericAux2PrimArg b1 b2 B.Prod' c
    recursive (BT.Infix' BT.Plus b1 b2) c = genericAux2PrimArg b1 b2 B.Add'  c
    recursive (BT.Infix' BT.Sub b1 b2)  c = genericAux2PrimArg b1 b2 B.Sub'  c
    recursive (BT.Infix' BT.Mod b1 b2)  c = genericAux2PrimArg b1 b2 B.Mod'  c
    recursive (BT.Infix' BT.Or b1 b2)   c = genericAux2PrimArg b1 b2 B.Or'   c
    recursive (BT.Infix' BT.And b1 b2)  c = genericAux2PrimArg b1 b2 B.And'  c
    recursive (BT.Infix' BT.Eq b1 b2)   c = genericAux2PrimArg b1 b2 B.Eq'   c
    recursive (BT.Infix' BT.Neq b1 b2)  c = genericAux2PrimArg b1 b2 B.Neq'  c
    recursive (BT.Infix' BT.Lt b1 b2)   c = genericAux2PrimArg b1 b2 B.Less' c
    recursive (BT.Infix' BT.Gt b1 b2)   c = genericAux2PrimArg b1 b2 B.More' c
    recursive (BT.Infix' BT.Ge b1 b2)   c = genericAux2PrimArg b1 b2 B.Meq'  c
    recursive (BT.Infix' BT.Le b1 b2)   c = genericAux2PrimArg b1 b2 B.Leq'  c
    recursive (BT.Infix' BT.Division b1 b2) c = genericAux2PrimArg b1 b2 B.Div' c
    recursive (BT.Cons b1 b2) c = genericAux2 (b1, Aux2) (b2, Aux1) (B.Cons', Prim) c
    recursive (BT.Symbol' s) context =
      case context Map.!? s of
        -- The symbol is bound, and thus we have a port and its number
        Just portInfo → chaseAndCreateFan portInfo
        -- The symbol is Free, just stash it in a symbol with no rewrite rules
        -- Note, that multiple instances of this symbol will create multiple
        -- Different symbols instead of sharing, as it is unbound, we can't do
        -- any processing anyways
        Nothing → (,) <$> (newNodeM (B.Symbol' s)) <*> pure Prim
    recursive (BT.Lambda s body) context = do
      numLam          ← newNodeM B.Lambda'
      (bNode,  bPort) ← recursive body (Map.insert s (numLam, Aux2) context)
      linkM (numLam, Aux1) (bNode, bPort)
      pure (numLam, Prim)
    recursive (BT.Let sym bound body) context = do
      (numBound, portBound) ← recursive bound context
      recursive body (Map.insert sym (numBound, portBound) context)
    recursive (BT.Letrec sym body) context = do
      numMu          ← newNodeM B.Mu'
      (bNode, bPort) ← recursive body (Map.insert sym (numMu, Aux2) context)
      linkM (numMu, Aux1) (bNode, bPort)
      pure (numMu, Prim)
    recursive (BT.If b1 b2 b3) c = do
      (numIf, retPort) ← genericAux2 (b1, Prim) (b2, Aux3) (B.IfElse', Aux1) c
      (b3Num, b3Port)  ← recursive b3 c
      linkM (numIf, Aux2) (b3Num, b3Port)
      pure (numIf, retPort)
    genericAux1 (b1, pb1) (langToCreate, portToReturn) context = do
      numCar        ← newNodeM langToCreate
      (bNum, bPort) ← recursive b1 context
      linkM (bNum, bPort) (numCar, pb1)
      pure (numCar, portToReturn)
    genericAux2 (b1, pb1) (b2, pb2) retInfo context = do
      (numApp, retPort) ← genericAux1 (b1, pb1) retInfo context
      (b2Num, b2Port)   ← recursive b2 context
      linkM (numApp, pb2) (b2Num, b2Port)
      pure (numApp, retPort)
    genericAux2PrimArg b1 b2 lc        = genericAux2 (b1, Prim) (b2, Aux2) (lc, Aux1)
    genericAux1PrimArg b1 langToCreate = genericAux1 (b1, Prim) (langToCreate, Aux1)

netToAst :: Net B.Lang → BT.Bohm
netToAst = undefined

-- Helper Functions-------------------------------------------------------------

-- | Creates a fan if the port is taken, and prepares to be connected
-- chaseAndCreateFan :: Net B.Lang → (Node, PortType) → (Node, Net B.Lang, PortType)
chaseAndCreateFan :: MonadState Env m ⇒ (Node, PortType) → m (Node, PortType)
chaseAndCreateFan (num,port) = do
  env ← get
  case findEdge (env^.net) num port of
    Nothing → pure (num, port)
    Just t1@(nConnected, connectedPort) → do
      env ← get
      put (over level succ env)
      numFan ← newNodeM (B.FanIn' (env^.level))
      let nodeFan = RELAuxiliary2 { node       = numFan
                                  , primary    = Link (Port port          num)
                                  , auxiliary1 = Link (Port connectedPort nConnected)
                                  , auxiliary2 = Link FreePort
                                  }
      modify (over net (\net → deleteEdge (linkAll net nodeFan) t1 (num,port)))
      pure (numFan, Aux2)
