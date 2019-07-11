import Juvix.Bohm.Translation
import Juvix.Bohm.Parser

import           Juvix.Nets.Bohm
import           Juvix.Interaction

import Protolude

test1 = runNet (reduceAll 10) . astToNet <$> parseBohm "(lambda x. (x x) y)"

test2 = runNet (reduceAll 10) . astToNet <$> parseBohm "((lambda x. (x x)) (lambda x. (x x)))"
