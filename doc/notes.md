https://github.com/cedille/cedille-core-spec/blob/master/spec.pdf
https://github.com/astump/cedilleum-spec/blob/master/spec/spec.pdf
https://github.com/cedille/cedille/tree/master/core

~

http://purelytheoretical.com/sywtltt.html


~~

> https://github.com/maiavictor/formality
> https://github.com/Gabriel439/Haskell-Morte-Library

Steps, roughly in order

https://github.com/idris-hackers/idris-koans
https://github.com/idris-hackers/idris-demos

# Reasoning

"Why is this the right language model"

- ERC20-esque token contract with verified behavior
- ERC721-esque token contract with verified behavior
- ERC??? account verification function standard with verified behavior
- Implementation of interesting voting system
- Wyvern reimplementation with verified behavior

Why (assert these are true!)
> More powerful (possible) compilation
    Actual optimization
    Easily multitarget
    Cross-contract compilation
> Safer contract-contract interaction
> Safer user experience

# PoC

"It works!"

- Reimplement Idris paper - http://sci-hub.tw/https://dl.acm.org/citation.cfm?doid=1929529.1929536
- Backend for Michelson that kinda works
- Rewrite all the Michelson lang example contracts
- Start optimizing Michelson backend based on gas costs

# V2

"It's (somewhat) useful!"

- Backend for Lotion (or similar)
- Backend for WASM => Cosmos SDK module with JIT WASM interpreter
- Write a VM, backend for the VM
