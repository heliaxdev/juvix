module Juvix.Bohm.Translation(astToNet, netToAst) where


import           Protolude hiding (link)
import qualified Data.Map.Strict as Map
-- eventually abstract out this import
import qualified Data.Graph.Inductive as G

import           Juvix.Interaction
import qualified Juvix.Nets.Bohm   as B
import qualified Juvix.Bohm.Type   as BT


astToNet :: BT.Bohm → Net B.Lang
astToNet bohm = finalNet
  where
    tupTriple (a,b) c = (a,b,c)
    (_,finalNet,_) = recursive bohm Map.empty (G.empty :: Net B.Lang)
    -- we return the port which the node above it in the AST connects to!
    recursive (BT.IntLit x) _context net = tupTriple (newNode net (B.IntLit' x)) Prim
    recursive BT.False'     _context net = tupTriple (newNode net B.Fals')       Prim
    recursive BT.True'      _context net = tupTriple (newNode net B.Tru')        Prim
    recursive BT.Nil        _context net = tupTriple (newNode net B.Nil')        Prim
    recursive (BT.Car b)     context net = genericAux1 (b, Prim)  (B.Car', Aux1)     context net
    recursive (BT.Cdr b)     context net = genericAux1 (b, Prim)  (B.Cdr', Aux1)     context net
    recursive (BT.IsNil b)   context net = genericAux1 (b, Prim)  (B.TestNil', Aux1) context net
    recursive (BT.Not b1)    context net = genericAux1 (b1, Prim) (B.Not', Aux1)     context net
    recursive (BT.Application b1 b2) c n = genericAux2 (b1, Prim) (b2, Aux2) (B.App', Aux1) c n
    recursive (BT.Symbol' s) context net =
      case context Map.!? s of
        -- The symbol is bound, and thus we have a port and its number
        Just portInfo → chaseAndCreateFan net portInfo
        -- The symbol is Free, just stash it in a symbol with no rewrite rules
        -- Note, that multiple instances of this symbol will create multiple
        -- Different symbols instead of sharing, as it is unbound, we can't do
        -- any processing anyways
        Nothing → tupTriple (newNode net (B.Symbol' s)) Prim
    recursive (BT.Lambda s body) context net =
      let (numLam, net')        = newNode net B.Lambda'
          (bNode, net'', bPort) = recursive body
                                            (Map.insert s (numLam, Aux2) context)
                                            net'
          net'''                = link net'' (numLam, Aux1) (bNode, bPort)
      in (numLam, net''', Prim)

    genericAux1 (b1, pb1) (langToCreate, portToReturn) context net =
      let (numCar, net')       = newNode net langToCreate
          (bNum, net'', bPort) = recursive b1 context net'
          net'''               = link net'' (bNum, bPort) (numCar, pb1)
      in (numCar, net''', portToReturn)
    genericAux2 (b1, pb1) (b2, pb2) (langToCreate, portToReturn) context net =
      let (numApp, net')           = newNode net langToCreate
          (b1Num, net'' , b1Port)  = recursive b1 context net'
          net'''                   = link net'' (numApp, pb1) (b1Num, b1Port)
          (b2Num, net'''', b2Port) = recursive b2 context net'''
          net'''''                 = link net'''' (numApp, pb2) (b2Num, b2Port)
      in (numApp, net''''', portToReturn)
netToAst :: Net B.Lang -> BT.Bohm
netToAst = undefined

-- Helper Functions-------------------------------------------------------------

-- | Creates a fan if the port is taken, and prepares to be connected
chaseAndCreateFan :: Net B.Lang → (Node, PortType) → (Node, Net B.Lang, PortType)
chaseAndCreateFan net (num,port) =
  case findEdge net num port of
    Nothing → (num, net, port)
    Just t1@(nConnected, connectedPort) →
      let (numFan, net') = newNode net B.FanIn'
          nodeFan        = RELAuxiliary2 { node       = numFan
                                         , primary    = Link (Port port          num)
                                         , auxiliary1 = Link (Port connectedPort nConnected)
                                         , auxiliary2 = Link FreePort
                                         }
          net''          = deleteEdge (linkAll net' nodeFan) t1 (num,port)
      in
        (numFan, net'', Aux2)
