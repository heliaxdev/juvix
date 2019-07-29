import           Data.Graph.Inductive

import           Juvix.Nets.Bohm
import           Juvix.Backends.Graph
import           Juvix.Backends.Interface
import           Juvix.Backends.Env
import           Juvix.Utility.Helper

test2 :: InfoNet (FlipNet Lang)
test2 = runFlipNet (reduceAll 10) (Flip (mkGraph [(1, Auxiliary2 (InfixB Eq)), (2, Primar (IntLit 2))]
                                                 [(1,2, (Edge (1, Prim) (2, Prim)))]))
