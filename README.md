
# An eDSL fusion framework based on monadic hylomorphisms

## Build

You can build it either as a ordinary Cabal project by `cabal build`,
or by the Makefile in the root (`make` or `make build`).

## Benchmarks

Execute `make bench` to benchmark the framework using the
*Floyd-Warshall Algorithm* provided by this repo (takes time!). The
results will be written in `/bench` directory, containing the
following files.

- `BenSt0PrStat.html`: Performance of program analyzers, without
  memoization, categorized by the existence of fusion (**F** for fusion
  and **NF** otherwise), for four examples.
- `BenSt0PrComp.html`: Performance of program generators, without
  memoization, categorized by the existence of fusion (**F** for fusion
  and **NF** otherwise), for four examples.
- `BenSt0MonStat.html`: Performance of program analyzers, with
  memoization, categorized by the existence of fusion (**F** for fusion
  and **NF** otherwise), for four examples.
- `BenSt0MonComp.html`: Performance of program generators, with
  memoization, categorized by the existence of fusion (**F** for
  fusion and **NF** otherwise), for four examples.
- `BenSt1.html`: Performance of the stage 1 program (i.e., the staged
  Floyd-Warshall algorithm). We compare programs that are
  straightforward staged, that are partially evaluated (tagged by
  **P**), and that are further memoized (tagged by **(P, M)**).

The benchmark is created by
[criterion](https://hackage.haskell.org/package/criterion), and you
can interact with the graph by clicking the bars.

## Tests

Execute `make test` to perform basic tests.
