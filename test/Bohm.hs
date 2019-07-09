import Juvix.Bohm.Translation
import Juvix.Bohm.Parser

import Protolude

test1 = astToNet <$> parseBohm "lambda x. (x x)"
