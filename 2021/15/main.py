"""Solutions on day 07.

https://adventofcode.com/2021/day/7
"""
from pathlib import Path
from typing import Tuple, Union, Iterator, Dict

import click
from tqdm import tqdm
import numpy as np
from heapq import heapify, heappush, heappop


def neighbors(point: Tuple[int], shp: Tuple[int]) -> Iterator[Tuple[int, int]]:
    """Iterate over all valide neigbhors of a point."""
    for k in [-1, 1]:
        i, j = point
        if (i+k >= 0) and (i+k < shp[0]):
            yield (i+k, j)

        if (j+k >= 0) and (j+k < shp[1]):
            yield (i, j+k)


def dijkstra(grid: np.ndarray) -> Tuple[int, Dict]:
    """Solve task 01.

    Using Dijkstra

    :param grid: the graph with node weights
    :returns: risk score
    """
    start = (0, 0)
    end = (grid.shape[0]-1, grid.shape[1]-1)

    visited = set()
    distance = {start: 0}

    heap = [(0, *start)]
    while len(heap) != 0:
        dist, i, j = heappop(heap)
        if (i, j) in visited:
            continue
        visited.add((i, j))
        for (m, n) in neighbors((i, j), grid.shape):
            new_dist = dist + grid[m, n]
            old_dist = distance.get((m, n), np.inf)
            if (m, n) in visited or old_dist <= new_dist:
                continue

            heappush(heap, (new_dist, m, n))
            distance[(m, n)] = new_dist

    return distance[end]


def build_larger_grid(grid: np.ndarray, n: int = 5) -> np.ndarray:
    """Repeat grid n times with shift."""
    # zero index everything
    template = grid - 1
    large_grid = []
    for i in range(n):
        row = []
        for j in range(n):
            row.append((template + i + j) % 9)

        large_grid.append(np.concatenate(row, axis=-1))

    large_grid = np.concatenate(large_grid, axis=0)
    return large_grid + 1


@click.command()
@click.argument('path', type=click.Path())
def main(path: Union[str, Path]):
    """Solve day 07 tasks.

    Read input from PATH and and apply solutions
    to day 07 tasks. Prints the solution.
    \f

    :param path: Path to the input file
    """
    path = Path(path)

    grid = []
    with open(path, 'r') as f:
        for line in f.readlines():
            grid.append([
                int(c)
                for c in line.strip()
            ])

    grid = np.array(grid, dtype='int')
    print(f'{grid.shape=}')

    print('\nTask 01')
    score = dijkstra(grid)
    print(f'{score=}')

    print('\nTask02')
    large_grid = build_larger_grid(grid)
    score = dijkstra(large_grid)
    print(f'{score=}')


if __name__ == '__main__':
    main()
