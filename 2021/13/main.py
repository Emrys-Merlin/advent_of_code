"""Solutions on day 13.

https://adventofcode.com/2021/day/13
"""
from pathlib import Path
from typing import Dict, List, Set, Tuple, Union

import click
import numpy as np
from tqdm import tqdm

DIRECTION_MAPPING = {'x': 0, 'y': 1}


def fold(
    coords: Set[Tuple[int, int]], folding_instructions: List[Tuple[int, int]]
) -> Tuple[List[int], Set[Tuple[int, int]]]:
    """Fold paper according to instructions.

    :param coords: Set of coordinates with points on them
    :folding_instructions: List with folding instruction in the form
    (direction(0, 1), folding_line)
    :returns: Tuple containing a list with the number of points after each
    folding instruction and the remaining coordinates after the final
    instruction
    """
    n_points = []
    for direction, n_line in tqdm(folding_instructions):
        new_coords = set()
        for coord in coords:
            if coord[direction] < n_line:
                new_coords.add(coord)
                continue

            coord = list(coord)
            coord[direction] = 2 * n_line - coord[direction]
            new_coords.add(tuple(coord))

        coords = new_coords
        n_points.append(len(coords))

    return n_points, coords


def coords2str(coords: Set[Tuple[int, int]]) -> str:
    """Print coordinates.

    Transforms the coordinates in same string representation
    as the example.
    :param coords: Set of coordinates
    :returns: String representation
    """
    coords = np.array(tuple(coords), dtype='int')
    x_max, y_max = coords.max(axis=0)
    grid = np.zeros([x_max + 1, y_max + 1])

    for x, y in coords:
        grid[x, y] = 1

    output = '\n'.join(
        [''.join(['#' if c == 1 else '.' for c in line]) for line in grid.T])

    return output


@click.command()
@click.argument('path', type=click.Path())
def main(path: Union[str, Path]):
    """Solve day 13 tasks.

    Read input from PATH and prints the solutions.
    \f

    :param path: Path to the input file
    """
    path = Path(path)

    first_half = True
    coords = []
    folding_instructions = []
    with open(path, 'r') as f:
        for line in f.readlines():
            line = line.strip()
            if len(line) == 0:
                first_half = False
                continue

            if first_half:
                coords.append(
                    tuple([int(number) for number in line.split(',')]))
            else:
                direction, n_line = line.split()[-1].split('=')
                n_line = int(n_line)
                direction = DIRECTION_MAPPING[direction]
                folding_instructions.append((direction, n_line))

        coords = set(coords)

        print(f'{len(coords)=}')
        print(f'{len(folding_instructions)=}')

        n_points, coords = fold(coords, folding_instructions)

        print('\nTask 01')
        print(f'{n_points[0]=}')

        print('\nTask 02')
        output = coords2str(coords)
        print(output)


if __name__ == '__main__':
    main()
