from collections import deque
from itertools import product
from pathlib import Path
from typing import Iterator

from loguru import logger
from typer import Typer
from utils import timer

main = Typer()

# Detect which connector types are admissible from the
# start position
VALID_START_CONNECTORS = {
    (-1, 0): {"|", "7", "F"},
    (0, 1): {"-", "7", "J"},
    (1, 0): {"|", "J", "L"},
    (0, -1): {"-", "F", "L"},
}

# Translate connector symbol to the two
# connecting neighbors
CONNECTOR_TO_NEIGHBOR = {
    "|": {(-1, 0), (1, 0)},
    "-": {(0, -1), (0, 1)},
    "7": {(0, -1), (1, 0)},
    "F": {(0, 1), (1, 0)},
    "J": {(0, -1), (-1, 0)},
    "L": {(0, 1), (-1, 0)},
}

# Introduce orientation on the
# 8-neighborhood of a point.
# This choice is clockwise ordering
ORIENTATION = [
    (-1, 0),
    (-1, 1),
    (0, 1),
    (1, 1),
    (1, 0),
    (1, -1),
    (0, -1),
    (-1, -1),
]

# Build a linked list for the orientation.
# Useful so we can easily follow the orientation
# from any staring neighbor
NEIGHBORHOOD_LINKED_LIST = {
    t1: t2 for t1, t2 in zip(ORIENTATION, ORIENTATION[1:] + ORIENTATION[:1])
}


class Grid:
    """Pipe grid abstraction"""

    def __init__(self, input: str) -> None:
        self.grid = [list(line) for line in input.splitlines()]
        self.start_position = self._start_position()

    def _start_position(self) -> tuple[int, int]:
        """Return the start position"""
        for i, row in enumerate(self.grid):
            for j, col in enumerate(row):
                if col == "S":
                    return i, j

        raise ValueError("No start position found")

    def _initialize_bfs(self) -> deque[tuple[int, int, int]]:
        """Return the neighbors of the start position

        Returns:
            Queue of neighbors of the start position
        """
        i, j = self.start_position
        queue = deque([])
        for di, dj in [(-1, 0), (0, 1), (1, 0), (0, -1)]:
            new_i = i + di
            new_j = j + dj
            if (
                0 <= new_i < len(self.grid)
                and 0 <= new_j < len(self.grid[0])
                and self.grid[new_i][new_j] in VALID_START_CONNECTORS[(di, dj)]
            ):
                queue.append((new_i, new_j, 1))

        return queue

    @timer
    def furthest_distance(self) -> int:
        """Return the furthest distance from the start position along pipe

        Implemented as breadth first search (BFS).
        """
        queue = self._initialize_bfs()
        assert len(queue) == 2

        visited = {self.start_position}
        max_distance = 1

        while len(queue) > 0:
            i, j, distance = queue.popleft()
            if (i, j) in visited:
                continue

            visited.add((i, j))
            max_distance = max(max_distance, distance)

            for di, dj in CONNECTOR_TO_NEIGHBOR[self.grid[i][j]]:
                if 0 <= i + di < len(self.grid) and 0 <= j + dj < len(self.grid[0]):
                    queue.append((i + di, j + dj, distance + 1))

        return max_distance

    @staticmethod
    def get_neighbors_in_order_from(i: int, j: int) -> Iterator[tuple[int, int]]:
        """Iterate neighbors in orientation

        Start from neighbor delta (i, j) and then go around clockwise.
        (i, j) is never yielded.

        Args:
            i: Starting neighbor row_idx_delta (i.e., -1, 0, 1)
            j: Starting neighbor col_idx_delta (i.e., -1, 0, 1)

        Yields:
            Other neighborhood deltas in clockwise order
        """
        new_i, new_j = NEIGHBORHOOD_LINKED_LIST[(i, j)]
        while new_i != i or new_j != j:
            yield new_i, new_j
            new_i, new_j = NEIGHBORHOOD_LINKED_LIST[(new_i, new_j)]

    def connected_component_seeds(
        self,
    ) -> tuple[set[tuple[int, int]], set[tuple[int, int]], set[tuple[int, int]]]:
        """Traverse path and assign neighbors to components

        Each point on the path has exactly 2 neighbors which are also on the path.
        We follow the path only in one direction and from the neighbor on the path
        that would be the next point, we enumerate all neighbors in clockwise order.
        All neighbors until we pass the opposite path segment belong to one component.
        The rest to the other. Afterward we have to clean the seeds for eventual path
        points.

        Returns:
            (path, component_seed1, component_seed2)
        """
        i, j = self.start_position
        q = self._initialize_bfs()
        # Choose path walk direction arbitrarily
        new_i, new_j, _ = q.popleft()
        # Delta to previous point on path
        delta = (i - new_i, j - new_j)

        path = {(i, j), (new_i, new_j)}
        component1: set[tuple[int, int]] = set()
        component2: set[tuple[int, int]] = set()

        while new_i != i or new_j != j:
            # Delta to next point on path
            di, dj = next(
                iter(CONNECTOR_TO_NEIGHBOR[self.grid[new_i][new_j]].difference([delta]))
            )

            # Look at all neighbors in clockwise direction starting from
            # the next path segment. All neighbors until we pass the opposite
            # path segment belong to one component the other to the other
            # component.
            first = True
            for neighbor in self.get_neighbors_in_order_from(di, dj):
                if neighbor == delta:
                    first = False
                    continue

                if first:
                    component1.add((new_i + neighbor[0], new_j + neighbor[1]))
                    continue

                component2.add((new_i + neighbor[0], new_j + neighbor[1]))

            # Continue along the path
            new_i += di
            new_j += dj
            path.add((new_i, new_j))
            delta = (-di, -dj)

        # Need to clean seeds for path segments
        return path, component1.difference(path), component2.difference(path)

    def fill_component(
        self, seed: set[tuple[int, int]], path: set[tuple[int, int]]
    ) -> set[tuple[int, int]]:
        """Fill the component seeds

        We use DFS to fill the component seeds bounded by the path and
        the grid boundaries.

        Args:
            seed: Seed points of component
            path: Path points

        Returns:
            Filled component
        """
        n_rows = len(self.grid)
        n_cols = len(self.grid[0])
        stack = list(seed)

        while len(stack) != 0:
            i, j = stack.pop()

            for di, dj in product([-1, 0, 1], repeat=2):
                if di == dj == 0:
                    continue

                new_i, new_j = i + di, j + dj

                if (
                    0 <= new_i < n_rows
                    and 0 <= new_j < n_cols
                    and (new_i, new_j) not in seed
                    and (new_i, new_j) not in path
                ):
                    stack.append((new_i, new_j))
                    seed.add((new_i, new_j))

        return seed

    @timer
    def count_inner_component(self) -> int:
        """Count the points enclosed by the loop

        Caution! This method only works if the path is a loop and
        the path does not fill all grid boundary points (i.e. all points
        (0, j), (n_rows - 1, j), (i, 0), (i, n_cols - 1)).

        Returns:
            Points in inner component
        """
        path, seed1, seed2 = self.connected_component_seeds()

        n_rows = len(self.grid)
        n_cols = len(self.grid[0])
        component1 = self.fill_component(seed1, path)
        if all(0 < i < n_rows - 1 and 0 < j < n_cols - 1 for i, j in component1):
            return len(component1)

        return len(self.fill_component(seed2, path))


@main.command()
def entrypoint(path: Path):
    """CLI entrypoint"""
    with open(path, "r") as f:
        input = f.read().strip()

    grid = Grid(input)

    logger.info(f"Task 01: {grid.furthest_distance()}")
    logger.info(f"Task 02: {grid.count_inner_component()}")


if __name__ == "__main__":
    main()
