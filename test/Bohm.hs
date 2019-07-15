import           Juvix.Bohm.Translation
import           Juvix.Bohm.Parser
import           Juvix.Nets.Bohm
import           Juvix.Interaction
import           Juvix.Visualize.Graph

import Protolude

test1 = runNet (reduceAll 10) . astToNet <$> parseBohm "(lambda x. (x x) y)"

test2 = runNet (reduceAll 10) . astToNet <$> parseBohm "((lambda x. (x x)) (lambda x. (x x)))"

test3 = runNet (reduceAll 1) . astToNet <$> parseBohm "((lambda x. (x x)) (lambda x. (x x)))"

test4 = runNet (reduceAll 10) . astToNet <$> parseBohm "(lambda y. (lambda x. (y x)) 2 3)"

printTestn n = showNet "test.dot" net
  where
    Right (InfoNet {net = net}) = n

printTest3 :: IO ()
printTest3 = showNet "test3.dot" net
  where
    Right (InfoNet {net = net}) = test3
