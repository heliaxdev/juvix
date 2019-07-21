import           Juvix.Bohm.Translation
import           Juvix.Bohm.Parser
import           Juvix.Nets.Bohm
import           Juvix.Visualize.Graph
import           Juvix.Backends.Graph
import           Juvix.Backends.Maps

import Protolude

--test0 = astToNet <$> parseBohm "(lambda x. (x x) y)"
test1 = runFlipNet (reduceAll 10) . astToNet <$> parseBohm "(lambda x. (x x) y)"


test2' = runMapNet (reduceAll 10) . astToNet <$> parseBohm "((lambda x. (x x)) (lambda x. (x x)))"
test2 = runFlipNet (reduceAll 10) . astToNet <$> parseBohm "((lambda x. (x x)) (lambda x. (x x)))"

test3 = runFlipNet (reduceAll 1) . astToNet <$> parseBohm "((lambda x. (x x)) (lambda x. (x x)))"

test4 = runFlipNet (reduceAll 10) . astToNet <$> parseBohm "(lambda y. (lambda x. (y x)) (lambda x. 2 + x))"

test5 = runFlipNet (reduceAll 10) . astToNet <$> parseBohm "(2 + 2)"

--printTestn n = showNet "test.dot" net
--  where
--    Right (InfoNet {net = net}) = n

--printTest3 :: IO ()
-- printTest3 = showNet "test3.dot" net
--  where
--    Right (InfoNet {net = net}) = test3
