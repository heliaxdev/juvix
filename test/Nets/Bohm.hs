import           Data.Graph.Inductive
import           Juvix.Nets.Bohm
import           Juvix.Interaction
import           Juvix.NodeInterface

test1 :: (Net Lang, StateInfo)
test1 = runNet (\net -> curryRule net (Eq', 1) (IntLit (Primary 1) 2, 2))
               (mkGraph [(1, Eq'), (2, IntLit' 2)] [(1,2, (Edge (1, Prim) (2, Prim)))])


test2 :: (Net Lang, StateInfo)
test2 = runNet (reduceAll 10) (mkGraph [(1, Eq'), (2, IntLit' 2)]
                                       [(1,2, (Edge (1, Prim) (2, Prim)))])
