# Juvix

Work in progress.

Install with ([Stack](https://haskellstack.org) required):

```bash
make
```

For full optimizations (but slower compile times):

```bash
make build-opt
```

Play around with:

```bash
./scripts/tezos_compile.sh [filename].idr -o [output].tz
```

For example, to compile [examples/add-two.idr](examples/add-two.idr), run:

```bash
./scripts/tezos_compile.sh examples/add-two.idr -o add-two.tz
```

To run a contract using tezos-client:

```bash
./scripts/tezos_run.sh [filename].tz [storage] [input] [amount]
```
