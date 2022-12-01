"""Solutions on day 17.

https://adventofcode.com/2021/day/17
"""
from pathlib import Path
from typing import Tuple, Union

import click
from math import sqrt, ceil


def task01(y_min: int) -> int:
    """Solve task 01.

    Caveat: This method actually only checks a necessary condition
    for maximal y velocity. It turned out to be enough for the task...

    :param y_min: The lower y-bound of the target area.
    :returns: Maximum probe height
    """
    v_y_max = abs(y_min) - 1
    max_height = v_y_max * (v_y_max + 1) // 2
    return max_height


def task02(x_range: Tuple[int, int], y_range: Tuple[int, int]) -> int:
    """Solve task 02.

    :param x_range: lower and upper x-bound of target area
    :param y_range: lower and upper y-bound of target area
    :returns: Number of valid starting directions
    """
    # Rough bounds for velocities
    v_x_min = ceil(-0.5 + sqrt(0.25 + 2 * x_range[0]))
    v_x_max = x_range[1]
    v_y_min = y_range[0]
    v_y_max = abs(y_range[0]) - 1

    # Brute force time...
    n = 0
    for v_x in range(v_x_min, v_x_max + 1):
        for v_y in range(v_y_min, v_y_max + 1):

            v = v_x
            x = 0
            y = 0
            while (x <= x_range[1]) & (y >= y_range[0]):
                if (x >= x_range[0]) & (y <= y_range[1]):
                    n += 1
                    break

                x += v
                y += v_y
                v = max(v - 1, 0)
                v_y -= 1

    return n


@click.command()
@click.argument("path", type=click.Path())
def main(path: Union[str, Path]):
    """Solve day 17 tasks.

    Read input from PATH and prints the solution.
    \f

    :param path: Path to the input file
    """
    path = Path(path)

    with open(path, "r") as f:
        line = f.readline().strip()

    splits = line.split()
    x = splits[-2][2:-1]
    y = splits[-1][2:]

    x_range = [int(number) for number in x.split("..")]
    y_range = [int(number) for number in y.split("..")]

    print(f"{x_range=}")
    print(f"{y_range=}")

    print("\nTask 01")
    max_height = task01(y_range[0])

    print(f"{max_height=}")

    print("\nTask02")
    n_start_directions = task02(x_range, y_range)
    print(f"{n_start_directions=}")


if __name__ == "__main__":
    main()
