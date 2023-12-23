from collections import defaultdict, deque
from dataclasses import dataclass
from pathlib import Path
from typing import Iterator

from loguru import logger
from typer import Typer
from utils import timer

main = Typer()


@dataclass
class Grid:
    grid: list[list[str]]
    start: tuple[int, int]
    end: tuple[int, int]

    @classmethod
    def from_input(cls, input: str) -> "Grid":
        grid = [list(line) for line in input.split("\n")]
        start = (0, 1)
        end = (len(grid) - 1, len(grid[0]) - 2)
        return cls(grid, start, end)

    @property
    def width(self) -> int:
        return len(self.grid[0])

    @property
    def height(self) -> int:
        return len(self.grid)

    def __getitem__(self, key: tuple[int, int]) -> str:
        return self.grid[key[0]][key[1]]

    def neighbors(
        self, row: int, col: int, dry: bool = False
    ) -> Iterator[tuple[int, int]]:
        """Get neighbors of a given cell.

        Args:
            row: row idx
            col: col idx
            dry: If true, ignore <, >, v, and ^. Defaults to False.

        Yields:
            Valid neighbor coords
        """
        candidates = [(0, 1), (0, -1), (1, 0), (-1, 0)]
        current = self[row, col]
        if not dry:
            if current == "<":
                candidates = [(0, -1)]
            elif current == ">":
                candidates = [(0, 1)]
            elif current == "^":
                candidates = [(-1, 0)]
            elif current == "v":
                candidates = [(1, 0)]

        for delta_row, delta_col in candidates:
            new_row = row + delta_row
            new_col = col + delta_col
            if (
                0 <= new_row < self.height
                and 0 <= new_col < self.width
                and self[new_row, new_col] != "#"
            ):
                yield (new_row, new_col)

    def find_longest_path(self, dry: bool = False) -> int:
        """Brute force longest path search.

        We walk all paths and take the longest one.

        Args:
            dry: If True, ignore <, >, v, and ^. Defaults to False.

        Returns:
            Longest path length
        """
        queue = deque([(self.start, {self.start})])

        distance = -1
        while queue:
            end_point, path = queue.popleft()

            if end_point == self.end:
                # -1 because we don't count the start point
                distance = max(distance, len(path) - 1)

            for neighbor in self.neighbors(*end_point, dry=dry):
                if neighbor not in path:
                    queue.append((neighbor, path | {neighbor}))

        return distance

    def find_crossings(
        self, dry: bool = False
    ) -> Iterator[tuple[tuple[int, int], int]]:
        """Find all crossings in the grid.

        A crossing is a cell that has more than 2 neighbors.

        Args:
            dry: If True, ignore <, >, v, and ^. Defaults to False.

        Yields:
            Crossing coords and number of neighbors
        """
        for row in range(self.height):
            for col in range(self.width):
                if self[row, col] == "#":
                    continue

                n_neighbors = len(list(self.neighbors(row, col, dry=dry)))
                if n_neighbors > 2:
                    yield (row, col), n_neighbors

    def bfs(
        self,
        start: tuple[int, int],
        invalid: tuple[int, int],
        ends: set[tuple[int, int]],
        dry: bool = False,
    ) -> list[tuple[int, int]]:
        """Breadth first search

        Args:
            start: Start coord
            invalid: Forbidden coord (generally the previous crossing)
            ends: All allowed end coords (generally all crossings)
            dry: If True, ignore <, >, v, and ^. Defaults to False.

        Returns:
            Path from start to end
        """
        queue = deque([(start, [start])])
        visited: set[tuple[int, int]] = set((invalid,))

        while queue:
            end_point, path = queue.popleft()

            if end_point in visited:
                continue

            visited.add(end_point)

            if end_point in ends:
                return path

            for neighbor in self.neighbors(*end_point, dry=dry):
                queue.append((neighbor, path + [neighbor]))

        return []

    def build_crossing_graph(self, dry: bool = False) -> dict[tuple[int, int], int]:
        """Build a graph of all crossings (and start and end)

        Args:
            dry: If True, ignore <, >, v, and ^. Defaults to False.

        Returns:
            Returns a dict of all crossings mapping to their connecting crossings
            and the distance between them.
        """
        direct_neighbors: dict[tuple[int, int], set[tuple[tuple[int, int], int]]] = {
            coord: set(self.neighbors(*coord, dry=dry))
            for coord, _ in self.find_crossings(dry=dry)
        }

        direct_neighbors[self.start] = {(self.start[0] + 1, self.start[1])}
        direct_neighbors[self.end] = {(self.end[0] - 1, self.end[1])}

        neighboring_crossings: dict[
            tuple[int, int], set[tuple[tuple[int, int], int]]
        ] = defaultdict(set)
        for coord, neighbors in direct_neighbors.items():
            while neighbors:
                neighbor = neighbors.pop()
                path = self.bfs(
                    start=neighbor,
                    invalid=coord,
                    ends=direct_neighbors.keys(),
                    dry=dry,
                )
                new_crossing = path[-1]
                if new_crossing not in direct_neighbors:
                    continue
                direct_neighbors[new_crossing].remove(path[-2])

                if new_crossing == coord:
                    continue

                neighboring_crossings[new_crossing].add((coord, len(path)))
                neighboring_crossings[coord].add((new_crossing, len(path)))

        return neighboring_crossings

    def find_longest_path_on_crossings(
        self, crossings_graph: dict[tuple[int, int], set[tuple[tuple[int, int], int]]]
    ) -> int:
        """Brute force longest path on crossings graph.

        Args:
            crossings_graph: The crossings graph

        Returns:
            Longest path length
        """
        queue = deque([(self.start, 0, {self.start})])

        distance = 0
        while queue:
            end_point, length, path = queue.popleft()

            if end_point == self.end:
                distance = max(distance, length)

            for neighbor, delta_length in crossings_graph[end_point]:
                if neighbor not in path:
                    queue.append((neighbor, length + delta_length, path | {neighbor}))

        return distance

    def __str__(self) -> str:
        return "\n".join("".join(row) for row in self.grid)


@timer
def task01(input: str) -> int:
    """Solution for task 01

    Args:
        input: Input string

    Returns:
        Longest path length (excluding start) without "dry" mode
    """
    return Grid.from_input(input).find_longest_path()


@timer
def task02(input: str) -> int:
    """Solution for task 02

    Args:
        input: Input string

    Returns:
        Longest path length (excluding start) with "dry" mode
    """
    grid = Grid.from_input(input)
    crossings_graph = grid.build_crossing_graph(dry=True)
    return grid.find_longest_path_on_crossings(crossings_graph)


@main.command()
def entrypoint(path: Path):
    with open(path, "r") as f:
        data = f.read().strip()

    logger.info(f"Task01: {task01(data)}")
    logger.info(f"Task02: {task02(data)}")


if __name__ == "__main__":
    main()
