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
./exec.sh [filename].idr -o [output].tz
```

For example, to compile [examples/add-two.idr](examples/add-two.idr), run:

```bash
./exec.sh examples/add-two.idr -o add-two.tz
```
