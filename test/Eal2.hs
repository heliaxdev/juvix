import qualified Data.Map.Strict  as Map
import           Juvix.Eal.Types2
import           Juvix.Library    hiding (Type, link, reduce)
import           Juvix.Eal.Eal2
import           Juvix.Eal.Solve


testGen = execWithAssignment testAssignment
        $ generateConstraints testTerm

res = runConstraints
    $ constraints
    $ snd
    $ testGen

cons = fst testGen
