"""Solutions on day 11.

https://adventofcode.com/2021/day/11
"""
from collections import deque
from math import prod
from pathlib import Path
from typing import List, Union

import click
import numpy as np
from tqdm import tqdm


def flash_step(grid: np.ndarray):
    """Perform a single grid update step.

    Update is performed in place.

    :param grid: Grid to update
    """
    grid += 1

    has_flashed = set()
    nodes = deque([(i, j) for i in range(grid.shape[0]) for j in range(grid.shape[1])])

    while len(nodes) != 0:
        i, j = nodes.popleft()

        if grid[i, j] <= 9 or (i, j) in has_flashed:
            continue

        has_flashed.add((i, j))

        for k, l in [
            (-1, -1),
            (-1, 0),
            (-1, 1),
            (0, -1),
            (0, 1),
            (1, -1),
            (1, 0),
            (1, 1),
        ]:
            if (
                0 > i + k
                or i + k >= grid.shape[0]
                or 0 > j + l
                or j + l >= grid.shape[0]
            ):
                continue
            grid[i + k, j + l] += 1
            nodes.append((i + k, j + l))

    grid[grid > 9] = 0
    return len(has_flashed)


def task01(grid: np.ndarray, n_iterations: int) -> List[int]:
    """Count flashes for each iteration.

    :param grid: Playing field
    :n_iterations: Number of iterations
    :returns: List with flashes per iteration
    """
    flashes = []
    for _ in tqdm(range(n_iterations)):
        n_flashes = flash_step(grid)
        flashes.append(n_flashes)

    return flashes


def task02(grid: np.ndarray) -> int:
    """Find synchronizing step.

    :param grid: Playing field
    :returns: Step at which flashes sync
    """
    grid_size = prod(grid.shape)
    counter = 0
    while True:
        n_flashes = flash_step(grid)
        counter += 1
        if n_flashes == grid_size:
            break

    return counter


@click.command()
@click.argument("path", type=click.Path())
@click.argument("n_iterations", type=click.INT)
def main(path: Union[str, Path], n_iterations: int):
    """Solve day 11 tasks.

    Read input from PATH and number of iterations N_ITERATIONS.
    Prints the solutions.
    \f

    :param path: Path to the input file
    """
    path = Path(path)

    grid = []
    with open(path, "r") as f:
        for line in f.readlines():
            grid.append([int(c) for c in line.strip()])

    grid = np.array(grid, dtype="int")
    grid2 = grid.copy()
    print(grid.shape)

    print("\nTask 01")
    n_flashes = task01(grid, n_iterations)
    print(f"{sum(n_flashes)=}")

    print("\nTask 02")
    sync_step = task02(grid2)
    print(f"{sync_step=}")


if __name__ == "__main__":
    main()
