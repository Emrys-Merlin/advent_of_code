"""Solutions on day 06.

https://adventofcode.com/2021/day/6
"""
from collections import Counter
from pathlib import Path
from typing import List, Union

import click
from tqdm import tqdm


def task01(fish: List[int], n_days: int) -> int:
    """Solves task 01.

    Naive implementation. Too slow for task 02.
    """
    for _ in tqdm(range(n_days), desc='Day'):
        n_new_fish = 0
        for i in range(len(fish)):
            if fish[i] != 0:
                fish[i] -= 1
                continue

            n_new_fish += 1
            fish[i] = 6

        fish += [8]*n_new_fish

    return len(fish)


def task02(fish: List[int], n_days: int) -> int:
    """Solves task 02.

    Solves task 02 as well as task 01.
    """
    fish = Counter(fish)

    for _ in tqdm(range(n_days), desc='Day'):
        new_fish = {}
        for day, n in fish.items():
            if day == 0:
                new_fish[6] = new_fish.get(6, 0) + n
                new_fish[8] = new_fish.get(8, 0) + n
                continue

            new_fish[day-1] = new_fish.get(day-1, 0) + n

        fish = new_fish

    return sum(new_fish.values())


@click.command()
@click.argument('path', type=click.Path())
@click.argument('n_days', type=click.INT)
def main(path: Union[str, Path], n_days: int):
    """Solve day 06 tasks.

    Read input from PATH and the N_DAYS to simulate.
    The tasks differ only in the number of days to
    simulate. Prints the solution.
    \f

    :param path: Path to the input file
    :param n_days: Number of days to simulate.
    """
    path = Path(path)

    with open(path, 'r') as f:
        for line in f.readlines():
            fish = [
                int(char)
                for char in line.strip().split(',')
            ]

    print(f'Initial number of fish: {len(fish)}')

    n_fish = task02(fish, n_days)

    print(f'{n_fish=}')


if __name__ == '__main__':
    main()
