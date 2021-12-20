"""Solutions on day 20.

https://adventofcode.com/2021/day/20
"""
from pathlib import Path
from typing import Tuple, Union

import click
import numpy as np
from tqdm import tqdm

MAPPING = {
    '#': 1,
    '.': 0
}


class Image:
    """Image with enhancement algorithm."""

    def __init__(self, grid: np.ndarray, enhancement: np.ndarray):
        """Initialize image.

        :param grid: Initial non-dark part of image
        :param enhancement: Enhancement algorithm
        """
        self.grid = grid
        self.enhancement = enhancement

        # Keep track of number of enhancements
        self.n_enhancements = 0
        # Value for the infinite number of pixels
        # outside the center
        self.boundary_value = 0

    def __repr__(self) -> str:
        """Return string representation of image"""
        res = ''
        for row in self.grid:
            res += ''.join([
                ' ' if pixel == 0 else '·'
                for pixel in row
            ])
            res += '\n'

        return res

    def __len__(self) -> int:
        """Return number of light pixels"""
        if self.boundary_value == 1:
            return np.inf
        return self.grid.sum()

    def shape(self) -> Tuple[int, int]:
        """Return current shape of image."""
        return self.grid.shape

    def enhance(self, n: int = 1):
        """Enhance image.

        :param n: Number of enhancements
        """
        for _ in tqdm(range(n)):
            self.n_enhancements += 1
            # Enlarge old grid with correct boundary values.
            old_grid = self.boundary_value*np.ones(
                (self.grid.shape[0]+4, self.grid.shape[1]+4),
                dtype='int')
            old_grid[2:-2, 2:-2] = self.grid
            # Create new grid
            new_grid = np.zeros_like(old_grid, dtype='int')

            # Iterate over all new positions
            for i in range(new_grid.shape[0]):
                for j in range(new_grid.shape[1]):
                    new_grid[i, j] = self._new_pixel(i, j, old_grid)

            # Update boundary value
            self.boundary_value = self.enhancement[
                int(''.join([str(self.boundary_value)]*9), 2)
            ]

            self.grid = new_grid

    def _new_pixel(self, i: int, j: int, old_grid: np.ndarray) -> int:
        """Compute new pixel value based on enhancement.

        :param i: first coordinate
        :param j: second coordinate
        :para old_grid: old grid to look up values
        """
        idx = 0
        for k in range(i-1, i+2):
            for m in range(j-1, j+2):
                if (
                        k < 0 or
                        m < 0 or
                        k >= old_grid.shape[0] or
                        m >= old_grid.shape[1]
                ):
                    bit = self.boundary_value
                else:
                    bit = old_grid[k, m]
                idx = 2*idx + bit

        return self.enhancement[idx]


@click.command()
@click.argument('path', type=click.Path())
def main(path: Union[str, Path]):
    """Solve day 20 tasks.

    Read input from PATH and prints the solution.
    \f

    :param path: Path to the input file
    """
    path = Path(path)

    grid = []
    with open(path, 'r') as f:
        for i, line in enumerate(f.readlines()):
            line = line.strip()
            if i == 0:
                enhancement = np.array([
                    MAPPING[c]
                    for c in line
                ], dtype='int')
            elif i > 1:
                grid.append([
                    MAPPING[c]
                    for c in line
                ])

    grid = np.array(grid, dtype='int')

    img = Image(grid, enhancement)
    print(f'{img.shape()=}')
    print(f'{len(img)=}')
    # print(img)

    print('\nTask 01')
    img.enhance(2)
    print(f'{len(img)=}')
    # print(img)

    print('\nTask 02')
    img.enhance(48)
    print(f'{len(img)=}')


if __name__ == '__main__':
    main()
