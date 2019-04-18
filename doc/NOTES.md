*Maia Victor / Formality / etc.*

1. Reading list (papers, roughly chronological)

    1. [An Algorithm for Optimal Lambda Calculus Reduction - John Lamping](https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.90.2386&rep=rep1&type=pdf)

       (notes)

    1. [Parallel beta reduction is not elementary recursive - Andrea Asperti](https://dl.acm.org/citation.cfm?id=268971&dl=ACM&coll=DL)

       (notes)

    1. [Interaction Combinators - Yves Lafont](https://www.sciencedirect.com/science/article/pii/S0890540197926432)

       (notes)

    1. [A Denotational Semantics for the Symmetric Interaction Combinators - Damiano Mazza](https://pdfs.semanticscholar.org/1731/a6e49c6c2afda3e72256ba0afb34957377d3.pdf)

       (notes)

    1. [A parallel implementation for optimal lambda-calculus reduction - Marco Pedicini](https://www.researchgate.net/publication/221336344_A_parallel_implementation_for_optimal_lambda-calculus_reduction)

       (notes)

    1. [Lambdascope: Another optimal implementation of the lambda calculus - Vincent van Oostrom](https://www.researchgate.net/publication/237723293_Lambdascope_Another_optimal_implementation_of_the_lambda-calculus)

       (notes)

    1. [A Unified Approach to Fully Lazy Sharing - Thibaut Balabonski](https://www.researchgate.net/publication/220997963_A_Unified_Approach_to_Fully_Lazy_Sharing)

       (notes)

    1. [Weak Optimality, and the Meaning of Sharing - Thibaut Balabonski](https://www.lri.fr/~blsk/Docs/Balabonski-WeakOptimality-ICFP13.pdf)

       (notes)

    1. [About the efficient reduction of lambda terms - Andrea Asperti](https://pdfs.semanticscholar.org/8a35/42d70fc1d4531bc77c7bda130b0350245763.pdf)

       (notes)

1. Blog posts

    1. [Solving the mystery behind Abstract Algorithm's magical optimizations](https://medium.com/@maiavictor/solving-the-mystery-behind-abstract-algorithms-magical-optimizations-144225164b07)

       (notes)

    1. [About induction on the calculus of constructions](https://medium.com/@maiavictor/about-induction-on-the-calculus-of-constructions-581fcfdb89c5)

       (notes)

    1. [The Symmetric Interaction Calculus](https://medium.com/@maiavictor/the-abstract-calculus-fe8c46bcf39c)

       (notes)

    1. [Towards a simple theorem prover](https://medium.com/@maiavictor/towards-a-simple-theorem-prover-5005a1e66a6f)

       (notes)


1. Code

    1. [Nasic](https://github.com/moonad/Nasic)

       (notes)

    1. [Elementary Affine Calculus](https://github.com/moonad/elementary-affine-calculus)

       (notes)

    1. [ESCoC](https://github.com/moonad/Formality-stdlib/tree/master/lib)

       (notes)

    1. [Formality](https://github.com/moonad/Formality)

       (notes)

*Cedille*

1. Papers

    1. [The Calculus of Dependent Lambda Eliminations](https://homepage.divms.uiowa.edu/~astump/papers/cdle.pdf)

       (notes)

    1. [From Realizability to Induction via Dependent Intersection](https://homepage.divms.uiowa.edu/~astump/papers/apal-2018.pdf)

       (notes)

    1. [Generic Zero-Cost Reuse for Dependent Types](https://homepage.divms.uiowa.edu/~astump/papers/icfp-2018.pdf)

       (notes)

1. Code

    1. [Cedille](https://github.com/cedille/cedille)

       (notes)

    1. [Cedille Developments](https://github.com/cedille/cedille-developments)

       (notes)

*Prior work*

1. Papers

    1. [Safer smart contracts through type-driven development](https://publications.lib.chalmers.se/records/fulltext/234939/234939.pdf)

       - The problems described here are mostly irrelevant; their approach fails to make use of trivial optimizations such as TCO and compiles to LLL instead of EVM opcodes.
       - The effects system is a reasonable design pattern.

    1. [Introduction to the Calculus of Inductive Constructions](https://hal.inria.fr/hal-01094195/document)

1. Resources

    1. [So you want to learn type theory](http://purelytheoretical.com/sywtltt.html)

    1. [Type theory podcast](http://typetheorypodcast.com/)

1. Code

    1. [Haskell Morte Library](https://github.com/Gabriel439/Haskell-Morte-Library)

    1. [Blodwen](https://github.com/edwinb/Blodwen)

    1. [Idris Codegen WASM](https://github.com/SPY/idris-codegen-wasm)

~~~

Find some "Intro to Type Theory" book (covering *notation*, at least); read it. 
Implement all of LambdaPi, including simple REPL, keep around for experimentation.
Read everything Maia Victor has done, email him with questions.
Construct a strategy; criteria/concerns/options:
  - Could be sovereign chain(s)
  - Could target multiple chains
  - Could have multiple IRs (compile & compare), even multiple frontends (though is that as useful?)
  - What are the primary constraints? Compiled speed, closures? Can some be addressed by restricting frontend syntax?
  - How can the language (especially the typechecker) be embedded within itself in a consistent manner?
  - How can execution be correctly priced in the Byzantine environment of blockchains? (maybe we must use another layer entirely - e.g. compile the compiler to WASM, then price gas in a WASM interpreter)
  - Can in some cases proofs of equivalance-under-execution be proved for hand-rolled optimizations, and can this be done schematically?

Toolchain stack aspects (to be done *after* type theoretical problems are addressed, since those are harder)
  - Equality by equivalence on-chain
  - Fused on-chain / off-chain / multi-chain / state channels in one "language", chains are first-class
  - plan / apply semantics instead of the Truffle garbage, code = config
  - Bridge separate frontend languages
  - SNARKS/STARKS/ZEXE/Aurora etc., R1CS targeting
  - Inclusion of game theoretic proofs, information theoretic proofs, etc. into the language itself
  - Supercompilation
  - Visualization, Luna-Lang

> Write http://dev.stephendiehl.com/fun/ but for Juvix
