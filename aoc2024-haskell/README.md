# aoc2024

[Advent of Code 2024](https://adventofcode.com/2024/) in Haskell

## run all days

```bash
$ cabal run
```

## build copy into root application directory

```bash
$ make
```

## run with args

```bash
$ make
$ ./aoc2024-haskell          # run all once
$ ./aoc2024-haskell -h       # display arg help
$ ./aoc2024-haskell -b       # run all 10 times each selecting the fastest
$ ./aoc2024-haskell -d 3     # run day 3 once
$ ./aoc2024-haskell -d 3 -b  # run day 3 10 times selecting the fastest
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
