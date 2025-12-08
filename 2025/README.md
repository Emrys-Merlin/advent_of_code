# My solutions for Advent of Code 2025

I decided to learn [Haskell](https://haskell.org/) with this year's AoC. Expect most solutions to be in Haskell. If I am very tight on time, I might resort to Python, but I try to avoid it.

## Solutions

| Day | 01 | 02 | 03 | 04 | 05 | 06 | 07 | 08 | 09 | 10 | 11 | 12 |
| --- | -- | -- | -- | -- | -- | -- | -- | -- | -- | -- | -- | -- |
| Task 01 | [![haskell]](./haskell/app/Day01.hs) | [![haskell]](./haskell/app/Day02.hs) | [![haskell]](./haskell/app/Day03.hs) | [![haskell]](./haskell/app/Day04.hs) | [![haskell]](./haskell/app/Day05.hs) | [![haskell]](./haskell/app/Day06.hs) | [![haskell]](./haskell/app/Day07.hs)[![ferris]](https://github.com/fspoettel/advent-of-code-2025/blob/main/src/bin/07.rs)[^1] | [![haskell]](./haskell/app/Day08.hs) | | | | |
| Task 02 | [![haskell]](./haskell/app/Day01.hs) | [![haskell]](./haskell/app/Day02.hs) | [![haskell]](./haskell/app/Day03.hs) | [![haskell]](./haskell/app/Day04.hs) | [![haskell]](./haskell/app/Day05.hs) | [![haskell]](./haskell/app/Day06.hs) | [![haskell]](./haskell/app/Day07.hs)[![ferris]](https://github.com/fspoettel/advent-of-code-2025/blob/main/src/bin/07.rs)[^1] | [![haskell]](./haskell/app/Day08.hs) | | | | |


The Haskell icon has been released into the public domain and can be found [here](https://commons.wikimedia.org/wiki/File:Haskell-Logo.svg).

The python icon is licensed under the GPL and can be found [here](https://en.m.wikipedia.org/wiki/File:Python-logo-notext.svg).

The ferris icon is licensed under CC0 and can be found [here](https://commons.wikimedia.org/wiki/File:Original_Ferris.svg).

## Structure

- [`examples`](./examples) contains the examples as separate files. The file names follow the format `day{day:02d}_{counter:02d}.txt`
- [`inputs`](./inputs) the same for the inputs, but without the counter, i.e., `day{day:02d}.txt`.
- The other directories are named after a programming language and contain the solutions to AoC in that programming language. This year, there is a [`Haskell`](./haskell) and (potentially) a [`python`](./python) directory. The READMEs in each directory tell you how to run the solutions.

[python]: https://upload.wikimedia.org/wikipedia/commons/c/c3/Python-logo-notext.svg
[haskell]: https://upload.wikimedia.org/wikipedia/commons/1/1c/Haskell-Logo.svg
[ferris]: https://upload.wikimedia.org/wikipedia/commons/0/0f/Original_Ferris.svg

[^1]: I pair-coded the Rust solution together with [@fspoettel](https://github.com/fspoettel).
