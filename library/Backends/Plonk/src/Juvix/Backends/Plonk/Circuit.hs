module Juvix.Backends.Plonk.Circuit where

import Text.PrettyPrint.Leijen.Text hiding ((<$>))
import Juvix.Library
-- | Arithmetic circuits without multiplication, i.e. circuits
-- describe affine transformations.
data AffineCircuit i f
  = Add (AffineCircuit i f) (AffineCircuit i f)
  | ScalarMul f (AffineCircuit i f)
  | ConstGate f
  | Var i
  deriving (Read, Eq, Show, Generic)

data Wire
  = InputWire Int
  | IntermediateWire Int
  | OutputWire Int
  deriving (Show, Eq, Ord, Generic)


instance Pretty Wire where
  pretty (InputWire v) = text "input_" <> pretty v
  pretty (IntermediateWire v) = text "imm_" <> pretty v
  pretty (OutputWire v) = text "output_" <> pretty v

newtype ArithCircuit f = ArithCircuit [Gate Wire f]
  deriving (Eq, Show, Generic)

instance Show f => Pretty (ArithCircuit f) where
  pretty (ArithCircuit gs) = vcat . map pretty $ gs

data Gate i f =
    RangeGate { 
        rangeVar :: AffineCircuit i f, 
        rangeNumBits :: Integer,
        rangeO :: i
        }
   | LookupGate { 
       lookupA :: Integer,
       lookupB :: Integer,
       lookupC :: Integer,
       lookupD :: Maybe Integer,
       lookupPi :: f,
       lookupO :: i
    }  
   | MulGate
       { mulL :: AffineCircuit i f,
        mulR :: AffineCircuit i f,
        mulO :: i
      }
    | BoolGate 
        { boolVar :: AffineCircuit i f,
          boolO :: i
        }
    | LogicGate
        { logicL :: i,
          logicR :: i,
          logicO :: i
        }
    | EqualGate
      { eqI :: i,
        eqM :: i,
        eqO :: i
      }
  deriving (Eq, Show, Generic)


instance (Pretty i, Show f) => Pretty (Gate i f) where
  pretty (MulGate l r o) =
    hsep
      [ pretty o,
        text ":=",
        parens (pretty l),
        text "*",
        parens (pretty r)
      ]


instance (Pretty i, Show f) => Pretty (AffineCircuit i f) where
  pretty = prettyPrec 0
    where
      prettyPrec :: (Pretty i, Show f) => Int -> AffineCircuit i f -> Doc
      prettyPrec p e =
        case e of
          Var v ->
            pretty v
          ConstGate f ->
            text $ show f
          ScalarMul f e1 ->
            text (show f) <+> text "*" <+> parensPrec 7 p (prettyPrec p e1)
          Add e1 e2 ->
            parensPrec 6 p $
              prettyPrec 6 e1
                <+> text "+"
                <+> prettyPrec 6 e2


parensPrec :: Int -> Int -> Doc -> Doc
parensPrec opPrec p = if p > opPrec then parens else identity