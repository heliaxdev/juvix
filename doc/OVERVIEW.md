Juvix V2
========

*A more elegant language for a more civilized age*

### Why

* Smart contracts change language economics - high assurance very important, optimization very important, complex invariants very important, compile times / workflow less important
* Greater opportunity to fund protocol-level ~ language-level development, less need to be sponsored by particular company use case
* Most necessary work already exists with some precedent in the academic literature, no need for novel theory (yet)

### Architectural Overview

* Based on a fork of [Idris](https://idris-lang.org) - keeping rough Idris typesystem and IR
* Interactive mode with direct interaction with base platforms, code-as-data
* Transpilation to JS, native code, and distributed ledger VMs alike - seamless marshaling/unmarshaling
* Supercompilation?
* Think about gas / execution time metering, look at what Zen Protocol is doing
* Quotation / calling compiler inline - necessary?
 
#### References

* https://eb.host.cs.st-andrews.ac.uk/writings/plpv11.pdf
* https://www.idris-lang.org/drafts/sms.pdf
* http://www.luna-lang.org/

### Supercompilation

#### Basics

#### Extensions

* Data-dependent / usage-dependent supercompilation over time? (a la "hotspot" in the JVM, some V8 features IIRC)

#### References

* https://eb.host.cs.st-andrews.ac.uk/drafts/impldtp.pdf
* https://ndmitchell.com/downloads/paper-rethinking_supercompilation-29_sep_2010.pdf
* http://repository.readscheme.org/ftp/papers/pe98-school/D-364.pdf
* https://themonadreader.files.wordpress.com/2014/04/super-final.pdf
* https://arxiv.org/abs/1005.5278
* http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.128.6414
* https://www.microsoft.com/en-us/research/people/simonpj/?from=http%3A%2F%2Fresearch.microsoft.com%2F~simonpj%2Fpapers%2Fsupercompilation
* https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/supercomp-by-eval.pdf

* https://www.cs.kent.ac.uk/people/staff/sjt/TTFP/ttfp.pdf
* http://www.cse.chalmers.se/research/group/logic/book/book.pdf

### Misc

* https://corecursive.com/006-type-driven-development-and-idris-with-edwin-brady
* https://www.quora.com/What-is-the-future-of-optimizing-compilers?share=1
* https://www.michelson-lang.com/contract-a-day.html#sec-1-33
> http://www.cse.chalmers.se/~peterd/papers/MartinL%C3%B6f1984.pdf
> https://hal.inria.fr/inria-00076024/document
> https://plfa.github.io/
> https://gdijkstra.github.io/thesis.pdf
