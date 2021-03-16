-- | Surface language
module Juvix.Backends.Plonk.Lang
  ( c,
    add,
    sub,
    mul,
    div,
    mod,
    exp_,
    and_,
    or_,
    xor_,
    not_,
    deref,
    cond,
    ret,
    compileWithWire,
  )
where

import Juvix.Backends.Plonk.Builder
import Juvix.Backends.Plonk.Circuit
import Juvix.Backends.Plonk.IR
import Juvix.Library hiding (div, mod)

-- | Convert constant to expression
c :: f -> IR i f f
c = IConst

-- | Binary arithmetic operations on expressions
add, sub, mul, div, mod :: IR i f f -> IR i f f -> IR i f f
add = IBinOp BAdd
sub = IBinOp BSub
mul = IBinOp BMul
div = IBinOp BDiv
mod = IBinOp BMod

-- | Exponent operation
exp_ :: Int -> IR i f f -> IR i f f
exp_ i = IUnOp (UExp i)

-- | Binary logic operations on expressions
and_, or_, xor_ :: IR i f Bool -> IR i f Bool -> IR i f Bool
and_ = IBinOp BAnd
or_ = IBinOp BOr
xor_ = IBinOp BXor

-- | Negate expression
not_ :: IR i f Bool -> IR i f Bool
not_ = IUnOp UNot

-- | Convert wire to expression
deref :: i -> IR i f f
deref = IVar

-- | Conditional statement on expressions
cond :: IR i f Bool -> IR i f ty -> IR i f ty -> IR i f ty
cond = IIf

-- | Return compilation of expression into an output wire
ret :: Num f => IR Wire f f -> IRM f Wire
ret = compileWithWire freshOutput

compileWithWire :: Num f => IRM f Wire -> IR Wire f f -> IRM f Wire
compileWithWire freshWire expr = do
  compileOut <- compile expr
  case compileOut of
    Left wire -> pure wire
    Right circ -> do
      wire <- freshWire
      emit $ MulGate (ConstGate 1) circ wire
      pure wire
