-- | Surface language
module Juvix.Backends.Plonk.Lang
  ( c,
    add,
    sub,
    mul,
    and_,
    or_,
    xor_,
    not_,
    deref,
    cond,
  )
where

import Juvix.Backends.Plonk.Expr
import Juvix.Library

-- | Convert constant to expression
c :: f -> Expr i f f
c = EConst

-- | Binary arithmetic operations on expressions
add, sub, mul, div, mod :: Expr i f f -> Expr i f f -> Expr i f f
add = EBinOp BAdd
sub = EBinOp BSub
mul = EBinOp BMul
div = EBinOp BDiv
mod = EBinOp BMod

-- | Binary logic operations on expressions
and_, or_, xor_ :: Expr i f Bool -> Expr i f Bool -> Expr i f Bool
and_ = EBinOp BAnd
or_ = EBinOp BOr
xor_ = EBinOp BXor

-- | Negate expression
not_ :: Expr i f Bool -> Expr i f Bool
not_ = EUnOp UNot

-- | Convert wire to expression
deref :: i -> Expr i f f
deref = EVar

-- | Conditional statement on expressions
cond :: Expr i f Bool -> Expr i f ty -> Expr i f ty -> Expr i f ty
cond = EIf
