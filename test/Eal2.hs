import           Juvix.Eal.Types2
import           Juvix.Library    hiding (Type, link, reduce)
import           Juvix.Eal.Eal2
import           Juvix.Eal.Solve
import           Juvix.Eal.Check

import qualified Data.Map.Strict as Map

testGen :: (RPT, Env)
testGen = execWithAssignment testAssignment
        $ generateConstraints testTerm

testGen' :: ((RPT, ParamTypeAssignment), Env)
testGen' = execWithAssignment testAssignment
        $ generateTypeAndConstraitns testTerm

checkAnswer :: IO (Either Errors RPT)
checkAnswer = validEal testTerm testAssignment

resAnswer :: IO (Maybe RPT)
resAnswer = do
  let (rtp,env) = testGen
  assignments    ← getConstraints (constraints env)
  return $ fmap (flip assignTerm rtp) assignments

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

-- Test term: \s . \z . s s z.
testTerm ∷ Term
testTerm = Lam (someSymbolVal "s")
             (Lam (someSymbolVal "z")
               (App (Var (someSymbolVal "s"))
                    (App (Var (someSymbolVal "s"))
                         (Var (someSymbolVal "z")))))

-- Test assignment - s : a → a, z : a.
testAssignment ∷ TypeAssignment
testAssignment = Map.fromList [
  (someSymbolVal "s", ArrT (SymT (someSymbolVal "a")) (SymT (someSymbolVal "a"))),
  (someSymbolVal "z", SymT (someSymbolVal "a"))
  ]
