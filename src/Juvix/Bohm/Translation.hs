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
    recursive (BT.Car b)     context net = genericAux1PrimArg b B.Car'     context net
    recursive (BT.Cdr b)     context net = genericAux1PrimArg b B.Cdr'     context net
    recursive (BT.IsNil b)   context net = genericAux1PrimArg b B.TestNil' context net
    recursive (BT.Not b)     context net = genericAux1PrimArg  b B.Not'    context net
    recursive (BT.Application b1 b2)    c n = genericAux2PrimArg b1 b2 B.App'  c n
    recursive (BT.Infix' BT.Mult b1 b2) c n = genericAux2PrimArg b1 b2 B.Prod' c n
    recursive (BT.Infix' BT.Plus b1 b2) c n = genericAux2PrimArg b1 b2 B.Add'  c n
    recursive (BT.Infix' BT.Sub b1 b2)  c n = genericAux2PrimArg b1 b2 B.Sub'  c n
    recursive (BT.Infix' BT.Mod b1 b2)  c n = genericAux2PrimArg b1 b2 B.Mod'  c n
    recursive (BT.Infix' BT.Or b1 b2)   c n = genericAux2PrimArg b1 b2 B.Or'   c n
    recursive (BT.Infix' BT.And b1 b2)  c n = genericAux2PrimArg b1 b2 B.And'  c n
    recursive (BT.Infix' BT.Eq b1 b2)   c n = genericAux2PrimArg b1 b2 B.Eq'   c n
    recursive (BT.Infix' BT.Neq b1 b2)  c n = genericAux2PrimArg b1 b2 B.Neq'  c n
    recursive (BT.Infix' BT.Lt b1 b2)   c n = genericAux2PrimArg b1 b2 B.Less' c n
    recursive (BT.Infix' BT.Gt b1 b2)   c n = genericAux2PrimArg b1 b2 B.More' c n
    recursive (BT.Infix' BT.Ge b1 b2)   c n = genericAux2PrimArg b1 b2 B.Meq'  c n
    recursive (BT.Infix' BT.Le b1 b2)   c n = genericAux2PrimArg b1 b2 B.Leq'  c n
    recursive (BT.Infix' BT.Division b1 b2) c n = genericAux2PrimArg b1 b2 B.Div' c n
    recursive (BT.Cons b1 b2) c n = genericAux2 (b1, Aux2) (b2, Aux1) (B.Cons', Prim) c n
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
    recursive (BT.Let sym bound body) context net =
      let (numBound, net', portBound) = recursive bound context net in
        recursive body (Map.insert sym (numBound, portBound) context) net'
    recursive (BT.Letrec sym body) context net =
      let (numMu, net') = newNode net B.Mu'
          (bNode, net'', bPort) = recursive body
                                            (Map.insert sym (numMu, Aux2) context)
                                            net'
          net'''                = link net'' (numMu, Aux1) (bNode, bPort)
      in (numMu, net''', Prim)
    recursive (BT.If b1 b2 b3) c n =
      let (numIf, net', retPort) = genericAux2 (b1, Prim) (b2, Aux3) (B.IfElse', Aux1) c n
          (b3Num, net'', b3Port) = recursive b3 c net'
          net'''                 = link net'' (numIf, Aux2) (b3Num, b3Port)
      in (numIf, net''', retPort)
    genericAux1 (b1, pb1) (langToCreate, portToReturn) context net =
      let (numCar, net')       = newNode net langToCreate
          (bNum, net'', bPort) = recursive b1 context net'
          net'''               = link net'' (bNum, bPort) (numCar, pb1)
      in (numCar, net''', portToReturn)
    genericAux2 (b1, pb1) (b2, pb2) retInfo context net =
      let (numApp, net', retPort) = genericAux1 (b1, pb1) retInfo context net
          (b2Num, net'', b2Port)  = recursive b2 context net'
          net'''                  = link net'' (numApp, pb2) (b2Num, b2Port)
      in (numApp, net''', retPort)
    genericAux2PrimArg b1 b2 lc        = genericAux2 (b1, Prim) (b2, Aux2) (lc, Aux1)
    genericAux1PrimArg b1 langToCreate = genericAux1 (b1, Prim) (langToCreate, Aux1)

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
