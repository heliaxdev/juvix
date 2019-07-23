import           Juvix.Bohm.Translation
import           Juvix.Bohm.Parser
import           Juvix.Nets.Bohm
import           Juvix.Visualize.Graph
import           Juvix.Backends.Graph
import           Juvix.Backends.Maps
import           Juvix.Backends.Env
import           Juvix.Utility.Helper
import           Juvix.Library
import           Juvix.Backends.Interface

import           Text.Parsec

test1 :: Either ParseError (InfoNet (FlipNet Lang))
test1 = runFlipNet (reduceAll 10) . astToNet <$> parseBohm "(lambda x. (x x) y)"


parsed :: Network net => Either ParseError (net Lang)
parsed = astToNet <$> parseBohm "((lambda x. (x x)) (lambda x. (x x)))"

test2' :: Either ParseError (InfoNet (Juvix.Backends.Maps.Net Lang))
test2' = runMapNet (reduceAll 10) . astToNet <$> parseBohm "((lambda x. (x x)) (lambda x. (x x)))"

test2 :: Either ParseError (InfoNet (FlipNet Lang))
test2 = runFlipNet (reduceAll 10) . astToNet <$> parseBohm "((lambda x. (x x)) (lambda x. (x x)))"

test3 :: Either ParseError (InfoNet (FlipNet Lang))
test3 = runFlipNet (reduceAll 1) . astToNet <$> parseBohm "((lambda x. (x x)) (lambda x. (x x)))"

test4 :: Either ParseError (InfoNet (FlipNet Lang))
test4 = runFlipNet (reduceAll 10) . astToNet <$> parseBohm "(lambda y. (lambda x. (y x)) (lambda x. 2 + x))"

test5 :: Either ParseError (InfoNet (FlipNet Lang))
test5 = runFlipNet (reduceAll 10) . astToNet <$> parseBohm "(2 + 2)"

printTestn :: Show b ⇒ Either a2 (InfoNet (FlipNet b)) → IO ()
printTestn n = showNet "test.dot" (runFlip net)
  where
    Right (InfoNet {net = net}) = n

printTest3 :: IO ()
printTest3 = showNet "test3.dot" (runFlip net)
  where
    Right (InfoNet {net = net}) = test3
