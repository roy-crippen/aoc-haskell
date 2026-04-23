# aoc2024

[Advent of Code 2024](https://adventofcode.com/2024/) in Haskell

## build and run `./aoc2024-haskell` in root application directory

```bash
$ make
$ ./aoc2024-haskell          # run all days once
$ ./aoc2024-haskell -h       # display arg help
$ ./aoc2024-haskell -b       # run all 10 times each selecting the fastest
$ ./aoc2024-haskell -d 3     # run day 3 once
$ ./aoc2024-haskell -d 3 -b  # run day 3 10 times selecting the fastest
```

## run with cabal

```bash
$ cabal run          # run all days once
$ cabal run -- -h    # display arg help
```

## repl for a particular day

```bash
# DayXX where XX is the day number
# example for day 1
$ cabal repl
> :l Day01
```

## repl for a particular day using example data

```bash
# DayXX where XX is the day number
# example for day 1
$ cabal repl --ghc-options="-DEXAMPLE"
> :l Day01
```
