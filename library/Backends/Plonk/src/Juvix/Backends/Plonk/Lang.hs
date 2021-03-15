-- | Surface language
module Juvix.Backends.Plonk.Lang
  ( c,
    add,
    sub,
    mul,
    div,
    mod,
    and_,
    or_,
    xor_,
    not_,
    deref,
    cond,
  )
where

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
