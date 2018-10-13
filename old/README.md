Juvix
=====

[![https://badges.frapsoft.com/os/mit/mit.svg?v=102](https://badges.frapsoft.com/os/mit/mit.svg?v=102)](https://opensource.org/licenses/MIT) [![Build Status](https://travis-ci.org/cwgoes/juvix.svg?branch=master)](https://travis-ci.org/cwgoes/juvix)

Juvix is an optimizing Haskell-to-Michelson source transpiler, designed to enable developers targeting the Tezos platform to make use of the expressive syntax and capable type system of the Haskell frontend. Using Juvix, you can write smart contracts directly in Haskell which are then converted into Michelson code to be executed on the Tezos blockchain.

Juvix supports the majority of the GHC featureset, including algebraic datatypes (implementation in progress), parametric polymorphism, ad-hoc polymorphism / typeclasses (with some limitations), do-notation, and most mundane syntax-transformation and typesystem extensions.

Released under the MIT license (see [LICENSE](LICENSE)). This is a proof-of-concept software implementation released without any guarantee of functionality, performance, or correctness.

A simple smart contract which swaps the storage and argument on each call: 

```haskell
module Swap where

import           Juvix.Lib

main ∷ (String, String) → (String, String)
main (param, storage) = (storage, param)
```

Syntax is Haskell standard; Juvix supports pattern matching, lexical variable capture and scoping, tuple/list sugar, etc. `Juvix.Lib` provides types, Michelson library calls, and utility functions - by virtue of utilizing the GHC frontend, Juvix supports the Haskell module import system. The `main` function must be monomorphic, although you need not explictly annotate it if a monomorphic type can be inferred.

Clone this repository, build Juvix using [Stack](https://haskellstack.org) - if you have Stack installed, `./juvix.sh build` will do it - save the above example to `swap.hs`, and compile it with `./juvix.sh compile swap.hs swap.tz`. Juvix will write a Michelson contract file to `swap.tz` which can then be run by the Tezos client.

To run a Haskell contract directly, first ensure you have the Tezos alphanet container running, then in a shell:

```bash
export PATH_TO_ALPHANET_SH="/home/cwgoes/working/crypto/tezos/alphanet.sh"
./juvix.sh run swap.hs '"storage"' '"param"'
```

When transpiling, Juvix will log notable transformations applied in nicely colored output.

Look in [doc](doc) for design rationale and future plans. For a few demonstrative example contracts, see [doc/EXAMPLES.md](doc/EXAMPLES.md).

Issues are welcome; I can be reached at `@cwgoes` on the Tezos Riot and would be glad to answer questions or help debug.

Read over the documentation/examples, try writing a smart contract or two, and let me know what you think!
