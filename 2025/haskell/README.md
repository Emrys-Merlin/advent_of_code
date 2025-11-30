# Haskell solutions to Advent of Code 2025

I decided to learn Haskell using AoC 2025. This endeavour can be found in this directory.

## Setup

For this project to find the puzzle inputs, they need to follow a certain format. Actual inputs for the day need to be in `../inputs/dayXX.txt`, where `XX` denotes the (zero-padded) day.
Example inputs live in `../examples/dayXX_YY.txt`, where `XX` denotes the day as before and `YY` indicates the number of the example (also zero-padded).

## Invocation

This project uses `cabal` as a build tool. To build it, run

```bash
cabal build
```

To run it, use

```bash
cabal run . -- DAY TASK [-e EXAMPLE]
```
where `DAY` and `TASK` specify the respective day and task. If `-e` is omitted, the solution is applied to the input file. Otherwise, the solution is applied to the respective example.
You can also get a help text via

```bash
cabal run . -- --help
```
