# Juvix

## Overview

[Idris](https://idris-lang.org) to [Michelson](https://tezos.gitlab.io/mainnet/whitedoc/michelson.html) compiler.

## Caveats

Work in progress. Do not expect API stability. This is experimental software and no warranty is provided or implied.

Expect Juvix to produce inefficient output Michelson code right now. Many more optimizations are possible (although some may require altering the core language).

## Installation

Install with ([Stack](https://haskellstack.org) required):

```bash
make
```

For full optimizations (but slower compile times):

```bash
make build-opt
```

## Usage

Play around with:

```bash
./scripts/tezos_compile.sh [filename].idr -o [output].tz
```

For example, to compile [examples/basics/add.idr](examples/basics/add.idr), run:

```bash
./scripts/tezos_compile.sh examples/add.idr -o add.tz
```

To run a contract using tezos-client:

```bash
./scripts/tezos_run.sh [filename].tz [storage] [input] [amount]
```

In combination:

```bash
./scripts/tezos_compile.sh examples/basics/add.idr -o add.tz
./scripts/tezos_run.sh add.tz 39 3
```

which should print

```bash
42
```
