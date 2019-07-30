import           Data.Map.Strict as Map
import           Juvix.Library   hiding (Sum, Product)

data Product = Product Product
             | Term
             | None
             deriving Show

data Sum = Branch SomeSymbol Product Sum
         | Single SomeSymbol Product
         deriving Show

data Name = Adt SomeSymbol Sum
          deriving Show

-- Replace with bohm later!
data Lambda = Lambda SomeSymbol Lambda
            | Value SomeSymbol
            | Application Lambda Lambda
            deriving Show

type Cases = [SomeSymbol]

data Bound = Bound { lam     :: Lambda
                   -- This is to remember what ADΤ we belong to
                   , adtName :: SomeSymbol
                   } deriving (Show, Generic)

data Env = Env { constructors :: Map SomeSymbol Bound
               -- | name is a mapping between the adt name and the ordered cases thereof
               , adtMap :: Map SomeSymbol Cases
               } deriving (Show, Generic)

data Errors = AlreadyDefined deriving Show

newtype EnvS a = EnvS (ExceptT Errors (State Env) a)
  deriving (Functor, Applicative, Monad)
  deriving (HasState "constructors" (Map SomeSymbol Bound)) via
    Field "constructors" () (MonadState (ExceptT Errors (State Env)))
  deriving (HasState "adtMap" (Map SomeSymbol Cases)) via
    Field "adtMap" () (MonadState (ExceptT Errors (State Env)))
  deriving (HasThrow "err" Errors) via
    MonadError (ExceptT Errors (State Env))

adtToMendler ∷ ( HasState "constructors" (Map SomeSymbol Bound) m
               , HasState "adtMap"       (Map SomeSymbol Cases) m
               , HasThrow "err"          Errors                 m )
             ⇒ Name → m ()
adtToMendler (Adt name s) = (sumRec s 0)
  where
    sumGen s p posAdt = do
      modify' @"adtMap" (Map.update (Just . (<> [s])) name)
      let prod = sumProd p posAdt
      cons     ← get @"constructors"
      case cons Map.!? s of
        Just _  → throw   @"err" AlreadyDefined
        Nothing → modify' @"constructors" (Map.insert s (Bound prod name))

    sumRec (Single s p) posAdt         = sumGen s p posAdt
    sumRec (Branch s p nextSum) posAdt = sumGen s p posAdt
                                       *> sumRec nextSum (succ posAdt)
    -- (numToIn posAdt prod)
    sumProd None posAdt = numToIn posAdt (Lambda (someSymbolVal "x")
                                                 (Value $ someSymbolVal "x"))
    sumProd Term posAdt = Lambda (someSymbolVal "%gen1")
                                 (numToIn posAdt (Value (someSymbolVal "%gen1")))
    sumProd (Product t1) posAdt = undefined t1 posAdt

numToIn :: Int → Lambda → Lambda
numToIn 0 arg = app in' (app inl arg)
numToIn n arg = app in' (rec' n (app inr arg))
  where
    rec' 1 acc = acc
    rec' n acc = rec' (pred n) (app inr acc)

userNat ∷ Name
userNat = Adt (someSymbolVal "Nat")
              (Branch (someSymbolVal "Z") None
                      (Single (someSymbolVal "S") Term))

-- Lambda Abstraction for mendler encoding -------------------------------------
inl ∷ Lambda
inl = Lambda x
           $ Lambda k
                  $ Lambda l
                         $ Application (Value k) (Value x)
      where
        x = someSymbolVal "x"
        k = someSymbolVal "k"
        l = someSymbolVal "l"


inr ∷ Lambda
inr = Lambda y
           $ Lambda k
                  $ Lambda l
                         $ Application (Value l) (Value y)
      where
        y = someSymbolVal "y"
        k = someSymbolVal "k"
        l = someSymbolVal "l"

foldM' ∷ Lambda
foldM' = Lambda alg $ Lambda d $ Application (Value d) (Value alg)
  where
    alg = someSymbolVal "alg"
    d   = someSymbolVal "d"

app ∷ Lambda → Lambda → Lambda
app (Lambda s t) replace = rec' t
  where
    rec' (Value s')
      | s == s'   = replace
      | otherwise = Value s'
    rec' (Lambda s' t)
      | s == s'   = Lambda s' t
      | otherwise = Lambda s' (rec' t)
    rec' (Application t1 t2) = Application (rec' t1) (rec' t2)
app t _replace = t

in' ∷ Lambda
in' = Lambda r $ Lambda f $ Application (app foldM' (Value f)) (Value r)
  where
    r = someSymbolVal "r"
    f = someSymbolVal "f"
