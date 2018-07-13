module Swap

import Prelude

%default total

main : (a, b) -> (b, a)
main (param, storage) = (storage, param)
