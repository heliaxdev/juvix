Aspects of Juvix

- Smart contract language
  - Fork Idris, make it better
  - Exploit differing cost tradeoffs in smart contracts
    - Correctness necessary
    - Compile time doesn't matter, small programs, can supercompile/search
    - Code reuse (even on-chain)

- Put the language in the consensus
  - Defined equalence semantics but implementation can change later
  - Contracts themselves can call the compiler (needs more R&D)
  - Bounties for proofs, sub-contract-upgrades, etc.

- Layer over consensus
  - Can run cross-chain over IBC
  - Targets multiple backends (Ethereum, Tezos, Cosmos) initially
  - Avoid lock-in, separate choice of application and choice of consensus
