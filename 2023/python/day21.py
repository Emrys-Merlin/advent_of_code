from collections import defaultdict, deque
from dataclasses import dataclass
from fractions import Fraction
from pathlib import Path
from typing import Iterator

from loguru import logger
from typer import Typer
from utils import timer

main = Typer()


@dataclass
class Grid:
    """Grid abstraction"""

    grid: list[list[str]]
    start: tuple[int, int]

    @classmethod
    def from_input(cls, input: str) -> "Grid":
        """Parse input"""
        grid: list[list[str]] = []
        start = (-1, -1)
        for row, line in enumerate(input.splitlines()):
            grid.append(list(line))
            col = line.find("S")
            if col != -1:
                start = (row, col)

        if start[0] == -1 or start[1] == -1:
            raise ValueError("No start found")

        return cls(grid, start)

    def __getitem__(self, pos: tuple[int, int]) -> str:
        row = pos[0]
        if row < 0:
            n = abs(row) // self.height + 1
            row += n * self.height
        row %= self.height
        col = pos[1]
        if col < 0:
            n = abs(col) // self.width + 1
            col += n * self.width
        col %= self.width
        return self.grid[row][col]

    @property
    def width(self) -> int:
        return len(self.grid[0])

    @property
    def height(self) -> int:
        return len(self.grid)

    @property
    def n_patches(self) -> int:
        return sum(char != "#" for line in self.grid for char in line)

    def __str__(self) -> str:
        return "\n".join("".join(row) for row in self.grid)

    def neighbors(
        self, pos: tuple[int, int], infinite: bool = False
    ) -> Iterator[tuple[int, int]]:
        """Classic neighbors

        Can work with an infinite grid

        Args:
            pos: start position
            infinite: If true, new neighbors can be outside the grid bound. Defaults to False.

        Yields:
            Admissible neighbors
        """
        row, col = pos
        for delta_row, delta_col in [(0, -1), (0, 1), (-1, 0), (1, 0)]:
            new_row, new_col = row + delta_row, col + delta_col
            if (
                infinite or (0 <= new_col < self.height and 0 <= new_col < self.width)
            ) and self[new_row, new_col] != "#":
                yield (new_row, new_col)

    def neighbors_with_glued_borders(
        self, coord: tuple[int, int], fields: set[tuple[int, int]]
    ) -> Iterator[tuple[tuple[int, int], set[tuple[int, int]]]]:
        """Alternative neighborhood computation with a single patch

        Assuming the ends are glued together and counting sheets.

        Args:
            coord: Start coord
            fields: Set of patch coord with the start coord

        Yields:
            neighbor coord and attached set of patch coords
        """
        row, col = coord
        for delta_row, delta_col in [(0, -1), (0, 1), (-1, 0), (1, 0)]:
            new_row, new_col = row + delta_row, col + delta_col
            f_delta_row = f_delta_col = 0
            if new_row < 0:
                new_row += self.height
                f_delta_row = -1
            elif new_row >= self.height:
                new_row -= self.height
                f_delta_row = 1

            if new_col < 0:
                new_col += self.width
                f_delta_col = -1
            elif new_col >= self.width:
                new_col -= self.width
                f_delta_col = 1

            if self[new_row, new_col] != "#":
                new_fields = {
                    (f_row + f_delta_row, f_col + f_delta_col)
                    for f_row, f_col in fields
                }

                yield ((new_row, new_col), new_fields)

    def walk_patches(self, n_steps: int = 64, infinite: bool = False) -> int:
        """BFS to walk the grid

        Args:
            n_steps: Number of steps. Defaults to 64.
            infinite: If True, repeat the grid. Defaults to False.

        Returns:
            n coords reachable at exactly n_steps
        """
        queue = deque([(self.start, 0)])
        visited: dict[tuple[int, int], int] = {}

        while queue:
            coord, steps = queue.popleft()

            if coord in visited:
                continue

            visited[coord] = steps

            if steps == n_steps:
                continue

            for neighbor in self.neighbors(coord, infinite=infinite):
                queue.append((neighbor, steps + 1))

        return sum(int((steps % 2) == (n_steps % 2)) for steps in visited.values())

    def walk_with_glued_borders(self, n_steps: int) -> int:
        """Adapted BFS to walk the grid with glued borders

        Args:
            n_steps: number of steps

        Returns:
            n coords reachable at exactly n_steps
        """
        # patch coord -> map coords
        patches: dict[tuple[int, int], set(tuple[int, int])] = {
            self.start: {(0, 0)},
        }
        for _ in range(n_steps):
            new_patches: dict[tuple[int, int], set(tuple[int, int])] = defaultdict(set)
            for coord, fields in patches.items():
                for neighbor, new_fields in self.neighbors_with_glued_borders(
                    coord,
                    fields,
                ):
                    new_patches[neighbor].update(new_fields)

            patches = new_patches

        return sum(len(current_map) for current_map in patches.values())

    def visualize(self, reached: set[tuple[int, int]]) -> str:
        """Visualize visited coord on the grid

        Args:
            reached: visited coordinates

        Returns:
            String representation
        """
        res = ""
        for row, line in enumerate(self.grid):
            for col, char in enumerate(line):
                if (row, col) in reached:
                    res += "O"
                else:
                    res += char
            res += "\n"

        return res


class Polynomial:
    """Degree 2 polynomial

    Based on reddit mega thread for day 21 solution.
    """

    def __init__(self, f: list[int]):
        """Define it via f(0), f(1), f(2)"""
        assert len(f) == 3

        self.a = Fraction(f[2] - 2 * f[1] + f[0], 2)
        self.b = Fraction((-f[2] + 4 * f[1] - 3 * f[0]), 2)
        self.c = Fraction(f[0])

    def __call__(self, x: int) -> int:
        res = self.a * x**2 + self.b * x + self.c
        assert res.denominator == 1
        return int(res)


@timer
def task01(input: str, n_steps: int = 64) -> int:
    """Solution for task 01

    Simple BFS

    Args:
        input: Input data
        n_steps: n_steps to go. Defaults to 64.

    Returns:
        n coords reachable at exactly n_steps
    """
    grid = Grid.from_input(input)
    return grid.walk_patches(n_steps=n_steps)


@timer
def task02(input: str, n_steps: int = 26501365) -> int:
    """Solution for task 02

    Based on reddit mega thread for day 21 solution.

    It is exploited that the number of visited coords
    is a degree 2 polynomial in n, with
    len(grid)*n + len(grid)//2 == n_steps. That's due
    to the free middle rows and cols and the free diagonals.


    Args:
        input: Input data
        n_steps: n_steps to go. Defaults to 26501365.

    Returns:
        n coords reachable at exactly n_steps
    """
    grid = Grid.from_input(input)
    assert grid.height == grid.width == 131
    half_grid = grid.height // 2

    # Initialize polynomial
    poly = Polynomial(
        [
            grid.walk_with_glued_borders(n_steps=half_grid + i * grid.width)
            for i in range(3)
        ]
    )

    # Evaluate polynomial
    assert (n_steps - half_grid) % grid.width == 0
    n = (n_steps - half_grid) // grid.width
    return poly(n)


@main.command()
def entrypoint(path: Path):
    with open(path, "r") as f:
        data = f.read().strip()

    logger.info(f"Task 01: {task01(data)}")
    logger.info(f"Task 02: {task02(data)}")


if __name__ == "__main__":
    main()
