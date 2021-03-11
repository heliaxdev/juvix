{-# LANGUAGE StandaloneDeriving #-}

module Juvix.Backends.Plonk.Expr where

import Juvix.Backends.Plonk.Circuit
import Juvix.Library
import Text.PrettyPrint.Leijen.Text hiding ((<$>))

-- pub fn add_witness_to_circuit_description(&mut self, value: BlsScalar) -> Variable {

-- pub fn with_expected_size(expected_size: usize) -> Self {

-- pub fn add_input(&mut self, s: BlsScalar) -> Variable {

-- pub fn lookup_gate(
--     &mut self,
--     a: Variable,
--     b: Variable,
--     c: Variable,
-- ) -> Result<(), PreProcessingError> {

-- pub fn poly_gate(
--     &mut self,
--     a: Variable,
--     b: Variable,
--     c: Variable,
--     q_m: BlsScalar,
--     q_l: BlsScalar,
--     q_r: BlsScalar,
--     q_o: BlsScalar,
--     q_c: BlsScalar,
--     pi: BlsScalar,
-- ) -> (Variable, Variable, Variable) {

--     pub fn constrain_to_constant(&mut self, a: Variable, constant: BlsScalar, pi: BlsScalar) {

--     pub fn assert_equal(&mut self, a: Variable, b: Variable) {

--     pub fn add_one_dummy_constraint(&mut self) {

--     pub fn add_dummy_constraints(&mut self) {

--     pub fn plookup_gate(
--         &mut self,
--         a: Variable,
--         b: Variable,
--         c: Variable,
--         d: Option<Variable>,
--         pi: BlsScalar,
--     )

-- pub fn range_gate(&mut self, witness: Variable, num_bits: usize) {

data UnOp f a where
  -- dup
  UDup :: UnOp f f
  -- iszero
  UIsZero :: UnOp f Bool
  UNot :: UnOp f Bool
  -- shl
  UShL :: Int -> UnOp f f
  -- shr
  UShR :: Int -> UnOp f f
  -- rotl
  URotL :: Int -> UnOp f f
  -- rotr
  URotR :: Int -> UnOp f f
  -- asserteq
  UAssertEq :: UnOp f Bool
  -- assertit
  UAssertIt :: UnOp f Bool

-- TODO: setpub
-- TODO: setpriv
-- TODO: mload
-- TODO: mstore

data BinOp f a where
  BAdd :: BinOp f f
  BSub :: BinOp f f
  BMul :: BinOp f f
  BDiv :: BinOp f f
  BMod :: BinOp f f
  BAnd :: BinOp f Bool
  BOr :: BinOp f Bool
  BXor :: BinOp f Bool

-- | Comparing operators
data CompOp f where
  -- gt
  CGt :: CompOp f
  -- gte
  CGte :: CompOp f
  -- lt
  CLt :: CompOp f
  -- lte
  CLte :: CompOp f
  -- eq
  CEq :: CompOp f

opPrecedence :: BinOp f a -> Int
opPrecedence BOr = 5
opPrecedence BXor = 5
opPrecedence BAnd = 5
opPrecedence BSub = 6
opPrecedence BAdd = 6
opPrecedence BDiv = 7
opPrecedence BMod = 7
opPrecedence BMul = 8

-- | Expression data type of (arithmetic) expressions over a field @f@
-- with variable names/indices coming from @i@.
data Expr i f ty where
  EConst :: f -> Expr i f f
  EVar :: i -> Expr i f f
  EUnOp :: UnOp f ty -> Expr i f ty -> Expr i f ty
  EBinOp :: BinOp f ty -> Expr i f ty -> Expr i f ty -> Expr i f ty
  ECompOp :: CompOp f -> Expr i f f -> Expr i f f -> Expr i f Bool
  EAcc :: [Expr i f ty] -> [ty] -> Expr i f ty
  EIf :: Expr i f Bool -> Expr i f ty -> Expr i f ty -> Expr i f ty
  EEccAdd :: Expr i f (f, f) -> Expr i f (f, f) -> Expr i f (f, f)

deriving instance (Show i, Show f, Show ty) => Show (Expr i f ty)

deriving instance (Show f) => Show (CompOp f)

deriving instance (Show f) => Show (BinOp f a)

deriving instance (Show f) => Show (UnOp f a)

instance Pretty (BinOp f a) where
  pretty op = case op of
    BAdd -> text "+"
    BSub -> text "-"
    BMul -> text "*"
    BAnd -> text "&"
    BOr -> text "|"
    BXor -> text "^"
    BDiv -> text "/"
    BMod -> text "%"

instance Pretty (CompOp f) where
  pretty op = case op of
    CGt -> text ">"
    CGte -> text ">="
    CLt -> text "<"
    CLte -> text "<="
    CEq -> text "=="


instance Pretty (UnOp f a) where
  pretty op = case op of
    UNot -> text "!"
    UDup -> text "dup"
    UIsZero -> text "0?"
    UShL _ -> text "<<"
    UShR _ -> text ">>"
    URotL _ -> text "<<<"
    URotR _ -> text ">>>"
    UAssertEq -> text "!"
    UAssertIt -> text "!"

instance (Pretty f, Pretty i, Pretty ty) => Pretty (Expr i f ty) where
  pretty = prettyPrec 0
    where
      prettyPrec :: (Pretty f, Pretty i, Pretty ty) => Int -> Expr i f ty -> Doc
      prettyPrec p e =
        case e of
          EVar v -> pretty v
          EConst l -> pretty l
          -- TODO correct precedence
          EUnOp op e1 -> parens (pretty op <+> pretty e1)
          EBinOp op e1 e2 ->
            parensPrec (opPrecedence op) p $
              prettyPrec (opPrecedence op) e1
                <+> pretty op
                <+> prettyPrec (opPrecedence op) e2
          EIf b true false ->
            parensPrec 4 p (text "if" <+> pretty b <+> text "then" <+> pretty true <+> text "else" <+> pretty false)

-------------------------------------------------------------------------------
-- Evaluator
-------------------------------------------------------------------------------

-- | Evaluate arithmetic expressions directly, given an environment
evalExpr ::
  (Bits f, Num f) =>
  -- | variable lookup
  (i -> vars -> Maybe f) ->
  -- | expression to evaluate
  Expr i f ty ->
  -- | input values
  vars ->
  -- | resulting value
  ty
evalExpr lookupVar expr vars = case expr of
  EConst f -> f
  EVar i -> case lookupVar i vars of
    Just v -> v
    Nothing -> panic "TODO: incorrect var lookup"
  EUnOp UNot e1 ->
    not $ evalExpr lookupVar e1 vars
  EUnOp (URotL rotBits) e1 -> notImplemented
  EUnOp (URotR rotBits) e1 -> notImplemented
  EBinOp op e1 e2 ->
    (evalExpr lookupVar e1 vars) `apply` (evalExpr lookupVar e2 vars)
    where
      apply = case op of
        BAdd -> (+)
        BSub -> (-)
        BMul -> (*)
        BAnd -> (&&)
        BOr -> (||)
        BXor -> \x y -> (x || y) && not (x && y)
  EIf b true false ->
    if evalExpr lookupVar b vars
      then evalExpr lookupVar true vars
      else evalExpr lookupVar false vars

-------------------------------------------------------------------------------
-- Circuit Builder
-------------------------------------------------------------------------------

-- type ExprM f a = State (ArithCircuit f, Int) a
-- @
-- data Foo = Foo {x, y :: 'Int'}
-- newtype M a = M ('State' 'Foo' a)
--   deriving ('HasState' \"x\" 'Int') via StateField "x" ('State' 'Foo')
-- @

data S f = S {sCircuit :: ArithCircuit f, sVarNum :: Int}
  deriving (Show, Generic)

newtype ExprM f a = ExprM {runExprM :: State (S f) a}
  deriving (Functor, Applicative, Monad)
  deriving
    ( HasState "sCircuit" (ArithCircuit f),
      HasSink "sCircuit" (ArithCircuit f),
      HasSource "sCircuit" (ArithCircuit f)
    )
    via StateField "sCircuit" (State (S f))
  deriving
    ( HasState "sVarNum" Int,
      HasSink "sVarNum" Int,
      HasSource "sVarNum" Int
    )
    via StateField "sVarNum" (State (S f))

execCircuitBuilder :: ExprM f a -> ArithCircuit f
execCircuitBuilder m = reverseCircuit . sCircuit $ execState (runExprM m) (S (ArithCircuit []) 0)
  where
    reverseCircuit = \(ArithCircuit cs) -> ArithCircuit $ reverse cs

evalCircuitBuilder :: ExprM f a -> a
evalCircuitBuilder = fst . runCircuitBuilder

runCircuitBuilder :: ExprM f a -> (a, ArithCircuit f)
runCircuitBuilder m = second (reverseCircuit . sCircuit) $ runState (runExprM m) (S (ArithCircuit []) 0)
  where
    reverseCircuit = \(ArithCircuit cs) -> ArithCircuit $ reverse cs

fresh :: ExprM f Int
fresh = do
  v <- get
    @"sVarNum"
  modify
    @"sVarNum" (+ 1)
  pure v

-- | Fresh intermediate variables
imm :: ExprM f Wire
imm = IntermediateWire <$> fresh

-- | Fresh input variables
freshInput :: ExprM f Wire
freshInput = InputWire <$> fresh

-- | Fresh output variables
freshOutput :: ExprM f Wire
freshOutput = OutputWire <$> fresh

-- | Multiply two wires or affine circuits to an intermediate variable
mulToImm :: Either Wire (AffineCircuit Wire f) -> Either Wire (AffineCircuit Wire f) -> ExprM f Wire
mulToImm l r = do
  o <- imm
  emit $ MulGate (addVar l) (addVar r) o
  pure o

-- | Add a Mul and its output to the ArithCircuit
emit :: Gate Wire f -> ExprM f ()
emit c = modify 
    @"sCircuit" (\(ArithCircuit cs) -> ArithCircuit (c : cs))

-- | Rotate a list to the right
rotateList :: Int -> [a] -> [a]
rotateList steps x = take (length x) $ drop steps $ cycle x

-- | Turn a wire into an affine circuit, or leave it be
addVar :: Either Wire (AffineCircuit Wire f) -> AffineCircuit Wire f
addVar (Left w) = Var w
addVar (Right c) = c

-- | Turn an affine circuit into a wire, or leave it be
addWire :: Num f => Either Wire (AffineCircuit Wire f) -> ExprM f Wire
addWire (Left w) = pure w
addWire (Right c) = do
  mulOut <- imm
  emit $ MulGate (ConstGate 1) c mulOut
  pure mulOut

compile :: Num f => Expr Wire f ty -> ExprM f (Either Wire (AffineCircuit Wire f))
compile expr = case expr of
  EConst n -> pure . Right $ ConstGate n
  EVar v -> pure . Left $ v
  EUnOp op e1 -> do
    e1Out <- compile e1
    case op of
      UNot -> pure . Right $ Add (ConstGate 1) (ScalarMul (-1) (addVar e1Out))
  -- URot truncBits rotBits -> do
  --   inp <- addi e1Out
  --   outputs <- replicateM truncBits imm
  --   emit $ Split inp outputs
  --   pure . Right $ unsplit (rotateList rotBits outputs)
  EBinOp op e1 e2 -> do
    e1Out <- addVar <$> compile e1
    e2Out <- addVar <$> compile e2
    case op of
      BAdd -> pure . Right $ Add e1Out e2Out
      BMul -> do
        tmp1 <- mulToImm (Right e1Out) (Right e2Out)
        pure . Left $ tmp1
      -- SUB(x, y) = x + (-y)
      BSub -> pure . Right $ Add e1Out (ScalarMul (-1) e2Out)
      BAnd -> do
        tmp1 <- mulToImm (Right e1Out) (Right e2Out)
        pure . Left $ tmp1
      BOr -> do
        -- OR(input1, input2) = (input1 + input2) - (input1 * input2)
        tmp1 <- imm
        emit $ MulGate e1Out e2Out tmp1
        pure . Right $ Add (Add e1Out e2Out) (ScalarMul (-1) (Var tmp1))
      BXor -> do
        -- XOR(input1, input2) = (input1 + input2) - 2 * (input1 * input2)
        tmp1 <- imm
        emit $ MulGate e1Out e2Out tmp1
        pure . Right $ Add (Add e1Out e2Out) (ScalarMul (-2) (Var tmp1))

      -- IF(cond, true, false) = (cond*true) + ((!cond) * false)
      EIf cond true false -> do
        condOut <- addVar <$> compile cond
        trueOut <- addVar <$> compile true
        falseOut <- addVar <$> compile false
        tmp1 <- imm
        tmp2 <- imm
        emit $ MulGate condOut trueOut tmp1
        emit $ MulGate (Add (ConstGate 1) (ScalarMul (-1) condOut)) falseOut tmp2
        pure . Right $ Add (Var tmp1) (Var tmp2)
-- EQ(lhs, rhs) = (lhs - rhs == 1)
-- EEq lhs rhs -> do
--   lhsSubRhs <- compile (EBinOp BSub lhs rhs)
--   eqIni <- addi lhsSubRhs
--   eqFreei <- imm
--   eqOuti <- imm
--   emit $ Equal eqIni eqFreei eqOuti
--   -- eqOuti == 0 if lhs == rhs, so we need to return 1 -
--   -- neqOuti instead.
--   pure . Right $ Add (ConstGate 1) (ScalarMul (-1) (Var eqOuti))

-- | Translate an arithmetic expression to an arithmetic circuit
exprToArithCircuit ::
  Num f =>
  -- | expression to compile
  Expr Int f ty ->
  -- | Wire to assign the output of the expression to
  Wire ->
  ExprM f ()
exprToArithCircuit expr output =
  exprToArithCircuit' (mapVarsExpr InputWire expr) output

exprToArithCircuit' :: Num f => Expr Wire f ty -> Wire -> ExprM f ()
exprToArithCircuit' expr output = do
  exprOut <- compile expr
  emit $ MulGate (ConstGate 1) (addVar exprOut) output

-- | Apply function to variable names.
mapVarsExpr :: (i -> j) -> Expr i f ty -> Expr j f ty
mapVarsExpr f expr = case expr of
  EVar i -> EVar $ f i
  EConst v -> EConst v
  EBinOp op e1 e2 -> EBinOp op (mapVarsExpr f e1) (mapVarsExpr f e2)
  EUnOp op e1 -> EUnOp op (mapVarsExpr f e1)
-- EIf b tr fl -> EIf (mapVarsExpr f b) (mapVarsExpr f tr) (mapVarsExpr f fl)
-- EEq lhs rhs -> EEq (mapVarsExpr f lhs) (mapVarsExpr f rhs)
