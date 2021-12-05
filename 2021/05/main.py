"""Solutions on day 05.

https://adventofcode.com/2021/day/5
"""
from pathlib import Path
from typing import Union

import click
import numpy as np
from tqdm import tqdm


def mark_axis_parallels(
        start_points: np.ndarray,
        end_points: np.ndarray
) -> np.ndarray:
    """Find mask for all non-diagonal lines."""
    diff = start_points - end_points
    mask = (diff == 0).max(axis=-1)
    return mask


def find_dangerous_spots(
        start_points: np.ndarray,
        end_points: np.ndarray
) -> int:
    """Compute number of lines at point and return occurence of at least 2."""
    # Build board that is large enough for all lines
    xs, ys = start_points.max(axis=0)
    xe, ye = end_points.max(axis=0)
    x_max = max(xs, xe)
    y_max = max(ys, ye)
    board = np.zeros((x_max+1, y_max+1))

    for start, end in zip(start_points, end_points):
        num = np.abs(end - start).max()
        direction = (end - start) // num
        for step in range(num+1):
            (x, y) = start + direction*step
            board[x, y] += 1

    return (board >= 2).sum()


def task01(start_points: np.ndarray, end_points: np.ndarray) -> int:
    """Solve task 01."""
    mask = mark_axis_parallels(start_points, end_points)
    start_points = start_points[mask]
    end_points = end_points[mask]

    return find_dangerous_spots(start_points, end_points)


def task02(start_points: np.ndarray, end_points: np.ndarray) -> int:
    """Solve task 02."""
    return find_dangerous_spots(start_points, end_points)


@click.command()
@click.argument('path', type=click.Path())
def main(path: Union[str, Path]):
    """Solve day 05 tasks.

    Read input from PATH and and apply solutions
    to day 04 tasks. Prints the solution (and the factors).
    \f

    :param path: Path to the input file
    """
    path = Path(path)

    start_points = []
    end_points = []
    with open(path, 'r') as f:
        for i, line in tqdm(enumerate(f.readlines())):
            line = line.strip()

            start, _, end = line.split()

            start_points.append([
                int(char)
                for char in start.split(',')
            ])
            end_points.append([
                int(char)
                for char in end.split(',')
            ])

    start_points = np.array(start_points, dtype='int')
    end_points = np.array(end_points, dtype='int')

    print(start_points.shape)
    print(end_points.shape)

    n_dangerous = task01(start_points, end_points)
    print(f'{n_dangerous=}')

    n_dangerous = task02(start_points, end_points)
    print(f'{n_dangerous=}')


if __name__ == '__main__':
    main()
