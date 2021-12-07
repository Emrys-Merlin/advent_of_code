"""Solutions on day 07.

https://adventofcode.com/2021/day/7
"""
from pathlib import Path
from typing import Callable, Tuple, Union

import click
import numpy as np
from tqdm import tqdm


def fuel_task01(positions: np.ndarray, final_position: int) -> int:
    """Metric for task 01.

    :param positions: All crab positions
    :param final_position: Final position of all crabs
    :returns: Fuel consumption for task 01
    """
    return np.abs(positions - final_position).sum()


def fuel_task02(positions: np.ndarray, final_position: int) -> int:
    """Metric for task 02.

    :param positions: All crab positions
    :param final_position: Final position of all crabs
    :returns: Fuel consumption for task 02
    """
    dist = np.abs(positions - final_position)
    return (dist*(dist+1)//2).sum()


def optimize_fuel(positions: np.ndarray, metric: Callable) -> Tuple[int, int]:
    """Optimize fuel for both tasks depending on metric.

    :param positions: The crab positions
    :param metric: Function to compute fuel consumption
    :returns: (minimal_fuel_consumption, corresponding_final_position)
    """
    left = positions.min()
    right = positions.max()

    fuel = np.inf
    minimal_pos = -1
    for pos in tqdm(range(left, right+1)):
        new_fuel = metric(positions, pos)
        if new_fuel < fuel:
            fuel = new_fuel
            minimal_pos = pos

    return fuel, minimal_pos


def task01(positions: np.ndarray) -> Tuple[int, int]:
    """Solves task 01.

    :param positions: Initial crab positions:
    :returns: (minimal_fuel_consumption, corresponding_final_position)
    """
    return optimize_fuel(positions, fuel_task01)


def task02(positions: np.ndarray) -> Tuple[int, int]:
    """Solves task 02.

    :param positions: Initial crab positions:
    :returns: (minimal_fuel_consumption, corresponding_final_position)
    """
    return optimize_fuel(positions, fuel_task02)


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
    with open(path, 'r') as f:
        line = f.readline()

    positions = np.array(line.strip().split(','), dtype='int')
    print(f'{len(positions)=}')
    print(f'{positions.min()=}')
    print(f'{positions.max()=}\n')

    print('Task 01')
    fuel, position = task01(positions)
    print(f'{fuel=}')
    print(f'{position=}\n')

    print('Task 02')
    fuel, position = task02(positions)
    print(f'{fuel=}')
    print(f'{position=}')


if __name__ == '__main__':
    main()
