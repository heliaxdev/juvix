-- Finite fields example
module FFExample where

import Data.Curve.Weierstrass.BLS12381 (Fr)
import Juvix.Backends.Plonk (FF (..), FFAnnTerm, FFType, PrimVal (..))
import qualified Juvix.Backends.Plonk as P
import Juvix.Core.ErasedAnn
import Juvix.Library (($), (.), Natural, undefined)
import Juvix.Library hiding (Type, exp)
import qualified Juvix.Library.NameSymbol as NameSymbol
import Juvix.Library.Usage

-- TODO: Is this signature correct?
sig :: FFType
sig =
  Pi (SNat 1) (PrimTy FF)
    $ Pi (SNat 1) (PrimTy FF)
    $ PrimTy FF

add, sub, mul, exp :: FFAnnTerm Fr
add = Ann Omega sig $ Prim PAdd
sub = Ann Omega sig $ Prim PSub
mul = Ann Omega sig $ Prim PMul
exp = Ann Omega sig $ Prim PExp

val :: Fr -> FFAnnTerm Fr
val = Ann (SNat 1) (PrimTy FF) . Prim . PConst

var :: NameSymbol.T -> FFAnnTerm Fr
var = Ann (SNat 1) (PrimTy FF) . Var

app :: FFAnnTerm Fr -> [FFAnnTerm Fr] -> FFAnnTerm Fr
app f xs = Ann (SNat 2) (PrimTy FF) $ AppM f xs

-- \x -> x^3 - 2x^2 + 4
erasedExample :: FFAnnTerm Fr
erasedExample =
  Ann Omega (PrimTy FF) $ LamM [] ["x"] $
    app
      sub
      [ app exp [var "x", val 3],
        app
          add
          [ app mul [val 2, app exp [var "x", val 2]],
            val 4
          ]
      ]

plonkExample :: P.IRM Fr P.Wire
plonkExample = do
  x <- P.deref <$> P.freshInput
  let xcube = P.exp_ 3 x
      xsq = P.mul (P.c 2) (P.exp_ 2 x)
  P.ret $ P.add (P.sub xcube xsq) (P.c 4)
