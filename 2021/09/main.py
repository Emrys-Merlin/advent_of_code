"""Solutions on day 09.

https://adventofcode.com/2021/day/9
"""
from math import prod
from pathlib import Path
from typing import List, Tuple, Union

import click
import numpy as np
from tqdm import tqdm


def task01(grid: np.ndarray) -> Tuple[int, List[Tuple[int, int]]]:
    """Find minima and compute risk.

    :param grid: Grid with heights
    :returns: Tuple of (risk, list of minima locations)
    """
    risk = 0
    minima = []
    for i in range(grid.shape[0]):
        for j in range(grid.shape[1]):
            mid = grid[i, j]
            north = grid[i - 1, j] if i - 1 >= 0 else np.inf
            south = grid[i + 1, j] if i + 1 < grid.shape[0] else np.inf
            east = grid[i, j + 1] if j + 1 < grid.shape[1] else np.inf
            west = grid[i, j - 1] if j - 1 >= 0 else np.inf

            if mid < min(north, east, south, west):
                risk += 1 + mid
                minima.append((i, j))

    return risk, minima


def task02(grid: np.ndarray, minima: List[Tuple[int, int]]) -> List[int]:
    """Return all basin sizes (same order as minima).

    Performs a DFS starting from each minimum until it reaches peaks with
    height 9.

    :param grid: Grid with heights
    :param minima: List of all minima locations in grid
    :returns: List with all basin sizes in same order as minima list
    """
    basin_sizes = []
    for i, j in tqdm(minima):
        visited = set()

        stack = [(i, j)]

        while len(stack) != 0:
            i, j = stack.pop()

            if grid[i, j] == 9:
                continue

            visited.add((i, j))

            if i >= 1 and (i - 1, j) not in visited:
                stack.append((i - 1, j))
            if i < grid.shape[0] - 1 and (i + 1, j) not in visited:
                stack.append((i + 1, j))
            if j >= 1 and (i, j - 1) not in visited:
                stack.append((i, j - 1))
            if j < grid.shape[1] - 1 and (i, j + 1) not in visited:
                stack.append((i, j + 1))

        basin_sizes.append(len(visited))

    return basin_sizes


@click.command()
@click.argument("path", type=click.Path())
def main(path: Union[str, Path]):
    """Solve day 09 tasks.

    Read input from PATH and prints the solutions.
    \f

    :param path: Path to the input file
    """
    path = Path(path)

    grid = []
    with open(path, "r") as f:
        for line in f.readlines():
            grid.append([int(c) for c in line.strip()])

    grid = np.array(grid, dtype="int")
    print(grid.shape)

    print("\nTask 01")
    risk, minima = task01(grid)
    print(f"{risk=}\n")

    print("Task 02")
    basin_sizes = task02(grid, minima)
    print(f"{basin_sizes=}")
    size_largest_three = prod(sorted(basin_sizes, reverse=True)[:3])
    print(f"{size_largest_three=}")


if __name__ == "__main__":
    main()
