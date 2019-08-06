import           Juvix.Eal.Types2
import           Juvix.Library    hiding (Type, link, reduce)
import           Juvix.Eal.Eal2
import           Juvix.Eal.Solve


testGen :: (RPT, Env)
testGen = execWithAssignment testAssignment
        $ generateConstraints testTerm

resMulti :: IO ()
resMulti = uncurry (flip (runMultipleConstraints 10))
         $ second constraints
         $ testGen

res :: IO ()
res = runConstraints
    $ constraints
    $ snd
    $ testGen

cons :: RPT
cons = fst testGen
