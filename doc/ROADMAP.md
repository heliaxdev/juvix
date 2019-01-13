Roadmap
=======

*Possible* future directions may include the following - feedback welcome.

**General**

* Improved typing (have GHC verify more of the invariants upon which compiler correctness depends; see source code for some notes on this)
* Internal architecture changes (related to above; also change compilation structure for easier optimization / analysis)
* Improved output Michelson code - annotate with function call stack, case statement position, etc.
* Test coverage (currently quite lacking) 
* Better optimization (see source code for ideas)

**Implement specific smart contracts**

* Simple tokens
* Futures/derivative contracts
* Prediction markets
* (open to other ideas)

**REPL / GHCi integration**

* GHCi-esque read-eval-print loop
* Integrate directly with Tezos chain-state using client API (i.e. provide contract simulation / deployment tools from within the interactive environment)
* Possible integration with Haskell a-la web3 but with native syntax, automatic marshaling/unmarshaling, integration into typesystem - this is fairly easy to architect, but would only be useful for Haskell programs

**Online editor / compiler**

* Analogous to [Ethereum Remix](https://remix.ethereum.org)
* Simple web IDE for compilation, debugging, deployment
* Convenient for experimentation, but I'm not sure how useful this is for serious contract development - personally I find Remix annoying and would far prefer better offline tooling

**Standard libraries**

* Michelson already fairly complete - but any additional common utility functionality should be standardized
* Not sure yet what will be commonly required

**Verification assistance, varying degrees of formality**

* Assist the smart contract programmer in proving certain properties of their program
* See e.g. https://ucsd-progsys.github.io/liquidhaskell-tutorial/02-logic.html
* Will require formal Michelson verification in order to provide complete assurance (correctness of Haskell is no guarantee of correctness of Michelson)
* May be possible to also prove certain things about more complex contracts including cryptography, see e.g. https://tamarin-prover.github.io/ (need to do further research on this)

**Visual editor**

* Tooling to visualization smart contract execution paths
* Potentially useful alternative way to catch bugs
* Partial inspiration: http://www.luna-lang.org/ (no affiliation)

**Dependent types**

* Support dependent type system (will require rewrite, although Haskell syntax can still mostly be used)
* e.g. [Idris](https://www.idris-lang.org/), [Agda](http://wiki.portal.chalmers.se/agda/pmwiki.php)
