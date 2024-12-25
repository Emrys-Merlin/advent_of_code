# My solutions for Advent of Code 2024

I decided together with some friends to learn [Rust](https://www.rust-lang.org/) with this year's AoC. So all except one task (day 24 task 2) were solved in Rust. For the one task, I ended up using a semi-manual approach using graph plotting (with graphviz). To speed up the plotting process, I decided to resort to python. I might add a rust solution in the futures.

## Solutions

| Day | 01 | 02 | 03 | 04 | 05 | 06 | 07 | 08 | 09 | 10 | 11 | 12 | 13 |
| --- | -- | -- | -- | -- | -- | -- | -- | -- | -- | -- | -- | -- | -- |
| Task 01 | [![ferris]](./rust/src/day01.rs) | [![ferris]](./rust/src/day02.rs) | [![ferris]](./rust/src/day03.rs) | [![ferris]](./rust/src/day04.rs) | [![ferris]](./rust/src/day05.rs) | [![ferris]](./rust/src/day06.rs) | [![ferris]](./rust/src/day07.rs) | [![ferris]](./rust/src/day08.rs) | [![ferris]](./rust/src/day09.rs) | [![ferris]](./rust/src/day10.rs) | [![ferris]](./rust/src/day11.rs) | [![ferris]](./rust/src/day12.rs) | [![ferris]](./rust/src/day13.rs) |
| Task 02 | [![ferris]](./rust/src/day01.rs) | [![ferris]](./rust/src/day02.rs) | [![ferris]](./rust/src/day03.rs) | [![ferris]](./rust/src/day04.rs) | [![ferris]](./rust/src/day05.rs) | [![ferris]](./rust/src/day06.rs) | [![ferris]](./rust/src/day07.rs) | [![ferris]](./rust/src/day08.rs) | [![ferris]](./rust/src/day09.rs) | [![ferris]](./rust/src/day10.rs) | [![ferris]](./rust/src/day11.rs) | [![ferris]](./rust/src/day12.rs) | [![ferris]](./rust/src/day13.rs) |

| Day |  14 | 15 | 16 | 17 | 18 | 19 | 20 |21 | 22 | 23 | 24 | 25 |
| --- |  -- | -- | -- | -- | -- | -- | -- |-- | -- | -- | -- | -- |
| Task 01 | [![ferris]](./rust/src/day14.rs) | [![ferris]](./rust/src/day15.rs) | [![ferris]](./rust/src/day16.rs) | [![ferris]](./rust/src/day17.rs) | [![ferris]](./rust/src/day18.rs) | [![ferris]](./rust/src/day19.rs) | [![ferris]](./rust/src/day20.rs) | [![ferris]](./rust/src/day21.rs) | [![ferris]](./rust/src/day22.rs) | [![ferris]](./rust/src/day23.rs) | [![ferris]](./rust/src/day24.rs) | [![ferris]](./rust/src/day25.rs) |
| Task 02 | [![ferris]](./rust/src/day14.rs) | [![ferris]](./rust/src/day15.rs) | [![ferris]](./rust/src/day16.rs) | [![ferris]](./rust/src/day17.rs) | [![ferris]](./rust/src/day18.rs) | [![ferris]](./rust/src/day19.rs) | [![ferris]](./rust/src/day20.rs) | [![ferris]](./rust/src/day21.rs) | [![ferris]](./rust/src/day22.rs) | [![ferris]](./rust/src/day23.rs) | [<img src="https://upload.wikimedia.org/wikipedia/commons/c/c3/Python-logo-notext.svg" width="18"/>](./python/src/aoc2024/day24.py) | :star: |

The ferris icon is licensed under CC0 and can be found [here](https://commons.wikimedia.org/wiki/File:Original_Ferris.svg).
The python icon is licensed under the GPL and can be found [here](https://en.m.wikipedia.org/wiki/File:Python-logo-notext.svg).

## Structure

- [`examples`](./examples) contains the examples as separate files. The file names follow the format `day{day:02d}_{counter:02d}.txt`
- [`inputs`](./inputs) the same for the inputs, but without the counter, i.e., `day{day:02d}.txt`.
- The other directories are named after a programming language and contain the solutions to AoC in that programming language. This year, there is a [`rust`](./rust) and a [`python`](./python) directory. The READMEs in each directory tell you how to run the solutions.

[ferris]: https://upload.wikimedia.org/wikipedia/commons/0/0f/Original_Ferris.svg
[python]: https://upload.wikimedia.org/wikipedia/commons/c/c3/Python-logo-notext.svg
