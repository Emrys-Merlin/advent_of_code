"""Solutions on day 03."""
import click
from pathlib import Path
from typing import Union, Tuple

import numpy as np


def binary_to_int(a: np.ndarray) -> int:
    """Transform binary array to (decimal) int."""
    res = 0
    for c in a:
        res = 2*res + c

    return res


def task01(matrix: np.ndarray) -> Tuple[int, int]:
    """Compute gamma and epsilon."""
    mn_count = matrix.mean(axis=0)

    gamma = mn_count > 0.5
    epsilon = 1 - gamma

    gamma = binary_to_int(gamma)
    epsilon = binary_to_int(epsilon)
    return gamma, epsilon


def sieve(matrix: np.ndarray, invert: bool = False) -> np.ndarray:
    """Sieve for oxygen and co2 rating.

    If invert is false, keep most common digit. Otherwise,
    keep least common.
    """
    for i in range(matrix.shape[1]):
        if len(matrix) == 1:
            break
        keep_ones = matrix[:, i].mean() >= 0.5
        keep_ones = (1 - invert)*keep_ones + invert*(1 - keep_ones)

        print(keep_ones)
        mask = matrix[:, i] == keep_ones
        matrix = matrix[mask]

    return np.squeeze(matrix)


def task02(matrix: np.ndarray) -> Tuple[int, int]:
    """Compute oxygen and co2 rating."""
    ogr = sieve(matrix)
    csr = sieve(matrix, invert=True)

    ogr = binary_to_int(ogr)
    csr = binary_to_int(csr)
    return ogr, csr


@click.command()
@click.argument('path', type=click.Path())
def main(path: Union[str, Path]):
    """Solve day 03 tasks.

    Read command file from PATH and and apply solutions
    to day 03 tasks. Prints the solution (and the factors).
    \f

    :param path: Path to the input file
    """
    path = Path(path)

    matrix = []
    with open(path, 'r') as f:
        for line in f.readlines():
            line = [
                int(char)
                for char in line.strip()
            ]
            matrix.append(line)

    matrix = np.array(matrix)

    gamma, epsilon = task01(matrix)
    print(f'{gamma=}\n{epsilon=}')
    print(f'Product: {gamma*epsilon}')

    ogr, csr = task02(matrix)
    print(f'{ogr=}\n{csr=}')
    print(f'Product: {ogr*csr}')


if __name__ == '__main__':
    main()
