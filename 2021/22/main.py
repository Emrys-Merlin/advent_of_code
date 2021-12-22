"""Solutions on day 21.

https://adventofcode.com/2021/day/21
"""
from collections import OrderedDict, namedtuple
from pathlib import Path
from typing import List, Optional, Tuple, Union

import click
import numpy as np

Instruction = namedtuple(
    'Instrucction',
    ['state', 'cuboid']
)

class Cube:
    """"""
    def __init__(self, ):
        """"""
        self.cube = np.zeros((8, 7))
        pass

    def __getitem__(self, *args, **kwargs):
        return self.cube.__getitem__(*args, **kwargs)

@click.command()
@click.argument('path', type=click.Path())
def main(path: Union[str, Path]):
    """Solve day 21 tasks.

    Read input from PATH and prints the solution.
    \f

    :param path: Path to the input file
    """
    path = Path(path)

    instructions = []
    cube_size = [lambda x: [np.inf, -np.inf] for x in range(3)]
    print(cube_size)
    with open(path, 'r') as f:
        for line in f.readlines():
            state, rest = line.strip().split()
            state = 1 if state == 'on' else 0
            cuboid = []
            for limits in rest.split(','):
                start, stop = limits[2:].split('..')
                limit = slice(int(start), int(stop)+1)
                cuboid.append(limit)

            instructions.append(Instruction(
                state=state,
                cuboid=tuple(cuboid)
            ))

    print(f'{len(instructions)=}')
    print(f'{instructions=}')


if __name__ == '__main__':
    main()
