import Juvix.Library hiding (Sum, Product)

data Product = Product Product Product
             | Term

data Sum = Branch SomeSymbol Product Sum
         | Single SomeSymbol Product

data Name = Adt SomeSymbol Sum

-- Replace with bohm later!
data Lambda = Lambda SomeSymbol Lambda
            | Value SomeSymbol
            | Application Lambda Lambda

userNat :: Name
userNat = Adt (someSymbolVal "nat")
              (Branch (someSymbolVal "Z") Term
                      (Single (someSymbolVal "S") Term))

