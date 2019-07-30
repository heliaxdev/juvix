{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import           Data.Map.Strict as Map
import           Juvix.Library   hiding (Sum, Product)
-- Adt Types -------------------------------------------------------------------

data Product = Product Product
             | Term
             | None
             deriving Show

data Sum = Branch SomeSymbol Product Sum
         | Single SomeSymbol Product
         deriving Show

data Name = Adt SomeSymbol Sum
          deriving Show

-- Object Syntax ---------------------------------------------------------------

-- Replace with bohm later!
data Lambda = Lambda SomeSymbol Lambda
            | Value SomeSymbol
            | Application Lambda Lambda
            deriving Show

-- In the real code bake this adt into Bohm or the Lambda itself!!!!

-- Case x of
-- /| Z   → body
-- /| S n → body
data Switch = Case Lambda

type Argument = SomeSymbol

type Body = Lambda

data Case = C SomeSymbol [Argument] Body

-- Environment type-------------------------------------------------------------
type Branches = [SomeSymbol]

data Bound = Bound { lam     :: Lambda
                   -- This is to remember what ADΤ we belong to
                   , adtName :: SomeSymbol
                   } deriving (Show, Generic)

data Env = Env { constructors :: Map SomeSymbol Bound
               -- | adtMap is a mapping between the adt name and the ordered cases thereof
               , adtMap :: Map SomeSymbol Branches
               } deriving (Show, Generic)

data Errors = AlreadyDefined deriving Show

newtype EnvS a = EnvS (ExceptT Errors (State Env) a)
  deriving (Functor, Applicative, Monad)
  deriving (HasState "constructors" (Map SomeSymbol Bound)) via
    Field "constructors" () (MonadState (ExceptT Errors (State Env)))
  deriving (HasState "adtMap" (Map SomeSymbol Branches)) via
    Field "adtMap" () (MonadState (ExceptT Errors (State Env)))
  deriving (HasThrow "err" Errors) via
    MonadError (ExceptT Errors (State Env))

-- Application code ------------------------------------------------------------
adtToMendler ∷ ( HasState "constructors" (Map SomeSymbol Bound)    m
               , HasState "adtMap"       (Map SomeSymbol Branches) m
               , HasThrow "err"          Errors                    m )
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

    sumProd None posAdt = numToIn posAdt (Lambda (someSymbolVal "x")
                                                 (Value $ someSymbolVal "x"))
    sumProd Term posAdt = Lambda (someSymbolVal "%gen1")
                                 (numToIn posAdt (Value (someSymbolVal "%gen1")))
    sumProd p@(Product _) posAdt =
      let (lambdas, term) = rec' p 0 (identity, (Value (someSymbolVal "%fun"))) in
        lambdas (numToInOp posAdt term)
      where
        rec' x index (lambdasBeforeIn, termToBuild) =
          let app = (Application termToBuild
                                 (Value (someSymbolVal ("%gen" <> show index))))
          in case x of
            Term →
              ( lambdasBeforeIn . Lambda (someSymbolVal ("%gen" <> show index))
              , Lambda (someSymbolVal "%fun") app)
            Product t  → rec' t
                              (succ index)
                              (lambdasBeforeIn . Lambda (someSymbolVal ("%gen" <> show index))
                              , app)
            None →
              ( lambdasBeforeIn
              , Lambda (someSymbolVal "%fun")
                         (Application
                          termToBuild
                          (Lambda (someSymbolVal "x")
                                  (Value $ someSymbolVal "x"))))

numToInGen :: Int → Lambda → Lambda
numToInGen 0 arg = app in' arg
numToInGen n arg = app in' (rec' n arg)
  where
    rec' 1 acc = acc
    rec' 2 acc = app inl acc
    rec' n acc = rec' (n - 2) (app inr acc)

numToIn :: Int → Lambda → Lambda
numToIn 0 arg = numToInGen 0 (app inl arg)
numToIn n arg = numToInGen n (app inr arg)

numToInOp :: Int → Lambda → Lambda
numToInOp 0 arg = numToInGen 0 (app inlOp arg)
numToInOp n arg = numToInGen n (app inrOp arg)

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

-- | Op of inl that has the first argument call the 2nd
-- useful for when constructing multiple argument passthrough
inlOp ∷ Lambda
inlOp = Lambda x
           $ Lambda k
                  $ Lambda l
                         $ Application (Value x) (Value k)
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

-- | Op of inr that has the first argument call the 2nd
-- useful for when constructing multiple argument passthrough
inrOp ∷ Lambda
inrOp = Lambda y
           $ Lambda k
                  $ Lambda l
                         $ Application (Value y) (Value l)
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
