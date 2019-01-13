module Tezos

import public Tezos.Prim

%access public export
%default total

data Operation

amount : Integer
amount = prim__tezosAmount

nil : List a
nil = prim__tezosNil
