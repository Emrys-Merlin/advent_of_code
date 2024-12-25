# Solutions for AoC 2024

This year I wanted to learn rust. So, all except one task were solved in rust only. I took a semi-manual approach for day24 part2 which required to plot a graph using graphviz. The tooling for python seemed easier than the one for rust, so I decided to go for python to speed up the process.

## Setup

Make sure you have [uv](https://docs.astral.sh/uv/getting-started/installation/) installed.


## Run an example

To run an example, you can simply run:
```shell
uv run aoc2024 <day> <task> -e <example_id>
```
For example, if you want to run the first example for day 13 task 2, you would enter

```shell
uv run aoc2024 13 2 -e 1
```

## Run a solution

To run a solution, you can simply leave out the `-e <example_id>`:
```shell
uv run aoc2024 <day> <task>
```
For example, if you want to run the solution for day 16 task 1, you would enter

```shell
uv run aoc2024 16 1
```

## Future directions

1. Adding unittests
2. Adding timing information
