"""Solutions on day 22.

https://adventofcode.com/2021/day/22
"""
from pathlib import Path
from typing import List, Tuple, Union
from collections import deque
from copy import deepcopy
from math import prod

import click
import numpy as np
from tqdm import tqdm
from dataclasses import dataclass
from sys import maxsize


@dataclass
class Instruction:
    """Container for reboot instructions."""

    state: int
    limits: np.ndarray


@dataclass
class Node:
    """Node of the cube tree."""

    depth: int
    threshold: int
    right_neighbor: 'Node'
    child: 'Node'
    state: int = 0

    def __repr__(self) -> str:
        """Representation of Node."""
        return f'Node({self.depth=}, {self.threshold=}, {self.state=})'

    def insert_right(self, threshold: int):
        """Insert node to the right.

        :param threshold: set threshold
        """
        child = deepcopy(self.child)
        new_node = Node(
            self.depth,
            threshold,
            self.right_neighbor,
            child,
            self.state
        )
        self.right_neighbor = new_node

    def compute_intervals(
            self,
            limits: np.ndarray,
            intervals: Tuple[int, int, int] = (0, 0, 0)
    ) -> Tuple[int, int, int]:
        """Compute interval length.

        :param limits: simits of the instruction cuboid
        :param intervals: old intervals
        :returns: new intervals
        """
        depth = self.depth
        neighbor = self.right_neighbor
        lower = max(limits[depth, 0], self.threshold)
        upper = min(limits[depth, 1], neighbor.threshold)
        interval = max(0, upper - lower)
        new_intervals = list(intervals)
        new_intervals[depth] = interval
        return tuple(new_intervals)


class Cube:
    """Reactor cube representation."""

    def __init__(self):
        """Initialize reactor cube."""
        z_node = Node(
            2,
            -maxsize,
            Node(2, maxsize, None, None),
            None
        )
        y_node = Node(
            1,
            -maxsize,
            Node(1, maxsize, None, None),
            z_node
        )
        self.tree = Node(
            0,
            -maxsize,
            Node(0, maxsize, None, None),
            y_node
        )

    def reboot(self, instructions: List[Instruction]):
        """Reboot reactor.

        :param instructions: List of activation instructions.
        """
        for instruction in tqdm(instructions):
            state = instruction.state
            limits = instruction.limits

            stack = deque([self.tree])

            while len(stack):
                node = stack.popleft()
                left_boundary_set = False

                while node.right_neighbor is not None:
                    depth = node.depth

                    if node.threshold >= limits[depth, 1]:
                        break

                    if left_boundary_set:
                        if node.right_neighbor.threshold > limits[depth, 1]:
                            node.insert_right(limits[depth, 1])

                        if depth == 2:
                            node.state = state
                        else:
                            stack.append(node.child)

                    else:
                        if node.right_neighbor.threshold >= limits[depth, 0]:
                            if node.right_neighbor.threshold > limits[depth, 0]:
                                node.insert_right(limits[depth, 0])

                            left_boundary_set = True

                    node = node.right_neighbor

    def count_active(self, limits: np.ndarray) -> int:
        """Count active centers within limits.

        :param limits: limit computation to range
        """
        intervals = self.tree.compute_intervals(limits, (0, 0, 0))
        stack = [(self.tree, intervals)]

        count = 0
        while len(stack):
            node, intervals = stack.pop()
            depth = node.depth
            state = node.state
            threshold = node.threshold

            new_node = node.right_neighbor
            if new_node.threshold < limits[depth, 1]:
                new_intervals = new_node.compute_intervals(limits, intervals)
                stack.append((new_node, new_intervals))

            if (
                    new_node.threshold < limits[depth, 0] or
                    threshold >= limits[depth, 1]
            ):
                continue

            if depth == 2:
                count += state*prod(intervals)
                continue

            new_intervals = node.child.compute_intervals(limits, intervals)
            stack.append((node.child, new_intervals))

        return count

    def __getitem__(self, location: Tuple[int, int, int]) -> int:
        """Get status of core at given location.

        :param location: (x, y, z) coordinate of core
        :returns: state (0 or 1)
        """
        node = self.tree
        depth = 0
        while True:
            value = location[depth]
            neighbor = node.right_neighbor
            if node.threshold <= value and value < neighbor.threshold:
                if depth == 2:
                    return node.state
                node = node.child
                depth += 1
                continue

            node = neighbor


@click.command()
@click.argument('path', type=click.Path())
def main(path: Union[str, Path]):
    """Solve day 22 tasks.

    Read input from PATH and prints the solution.
    \f

    :param path: Path to the input file
    """
    path = Path(path)

    instructions = []
    cube_limits = np.array(list(zip([np.inf]*3, [-np.inf]*3)))
    with open(path, 'r') as f:
        for line in f.readlines():
            state, rest = line.strip().split()
            state = 1 if state == 'on' else 0
            cuboid = []
            for i, limits in enumerate(rest.split(',')):
                start, stop = limits[2:].split('..')
                start, stop = int(start), int(stop) + 1
                limit = (start, stop)
                cuboid.append(limit)

                if start < cube_limits[i, 0]:
                    cube_limits[i, 0] = start

                if stop > cube_limits[i, 1]:
                    cube_limits[i, 1] = stop

            instructions.append(Instruction(
                state=state,
                limits=np.array(cuboid)
            ))

    cube_limits = cube_limits.astype('int')

    print(f'{len(instructions)=}')

    cube = Cube()
    cube.reboot(instructions)

    print('\nTask 01')
    limits = np.array(list(zip([-50]*3, [51]*3)))
    active_cores = cube.count_active(limits)
    print(f'{active_cores=}')

    print('\nTask 02')
    active_cores = cube.count_active(cube_limits)
    print(f'{active_cores=}')


if __name__ == '__main__':
    main()
