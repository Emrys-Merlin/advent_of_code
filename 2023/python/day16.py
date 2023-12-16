from dataclasses import dataclass
from pathlib import Path
from typing import Literal

from loguru import logger
from typer import Typer
from utils import timer

main = Typer()

DIRECTION: dict[str, tuple[int, int]] = {
    "N": (-1, 0),
    "E": (0, 1),
    "S": (1, 0),
    "W": (0, -1),
}

# Direction change via mirror
MIRRORS: dict[str, dict[str, str]] = {
    "N": {"\\": "W", "/": "E"},
    "E": {"\\": "S", "/": "N"},
    "S": {"\\": "E", "/": "W"},
    "W": {"\\": "N", "/": "S"},
}

# Direction change via splitter
SPLITTERS: dict[str, dict[str, list[str]]] = {
    "N": {"-": ["E", "W"], "|": ["N"]},
    "E": {"-": ["E"], "|": ["N", "S"]},
    "S": {"-": ["E", "W"], "|": ["S"]},
    "W": {"-": ["W"], "|": ["N", "S"]},
}


@dataclass(frozen=True)
class Beam:
    """Beam of light"""

    row: int
    col: int
    direction: Literal["N", "E", "S", "W"]

    @property
    def coord(self) -> tuple[int, int]:
        return (self.row, self.col)

    def on_grid(self, grid: "Grid") -> bool:
        """Is the beam on the grid?"""
        return 0 <= self.row < grid.height and 0 <= self.col < grid.width

    def interaction(self, obj: str) -> list["Beam"]:
        """Interaction with object

        Args:
            obj: Any of ".", "/", "\", "-", "|"

        Returns:
            Returns a list of beams after the interaction
        """
        if obj == ".":
            directions = [self.direction]

        elif obj in {"/", "\\"}:
            directions = [MIRRORS[self.direction][obj]]

        else:
            directions = SPLITTERS[self.direction][obj]

        return [
            Beam(
                self.row + DIRECTION[direction][0],
                self.col + DIRECTION[direction][1],
                direction,
            )
            for direction in directions
        ]


@dataclass
class Grid:
    """Grid of objects"""

    grid: list[list[str]]

    @classmethod
    def from_input(cls, input: str) -> "Grid":
        return cls([list(line) for line in input.splitlines()])

    @property
    def width(self) -> int:
        return len(self.grid[0])

    @property
    def height(self) -> int:
        return len(self.grid)

    def __getitem__(self, row: int, col: int) -> str:
        return self.grid[row][col]

    def follow_beam(self, beam: Beam = Beam(0, 0, "E")) -> int:
        """Follow the beam on the grid

        Args:
            beam: Starting position and direction of the beam. Defaults to Beam(0, 0, "E").

        Returns:
            Number of grid points visited by the beam
        """
        stack = [beam]
        visited: set[Beam] = set()

        while stack:
            beam = stack.pop()

            if not beam.on_grid(self) or beam in visited:
                continue

            visited.add(beam)

            obj = self.grid[beam.row][beam.col]
            stack.extend(beam.interaction(obj))

        return len({beam.coord for beam in visited})


@timer
def task01(input: str) -> int:
    """Solution for task 01

    Args:
        input: Input string

    Returns:
        Returns the number of grid points visited by the beam
    """
    grid = Grid.from_input(input)
    return grid.follow_beam()


@timer
def task02(input: str) -> int:
    """Brute force solution for task 02

    Args:
        input: Input string

    Returns:
        Maximum number for grid points visited by the beam starting from
        any of the edge points.
    """
    res = 0
    grid = Grid.from_input(input)

    for row in range(grid.height):
        res = max(
            res,
            grid.follow_beam(Beam(row, 0, "E")),
            grid.follow_beam(Beam(row, grid.width - 1, "W")),
        )

    for col in range(grid.width):
        res = max(
            res,
            grid.follow_beam(Beam(0, col, "S")),
            grid.follow_beam(Beam(grid.height - 1, col, "N")),
        )

    return res


@main.command()
def entrypoint(path: Path):
    """CLI entrypoint"""
    with open(path, "r") as f:
        input = f.read().strip()

    logger.info(f"Task01: {task01(input)}")
    logger.info(f"Task02: {task02(input)}")


if __name__ == "__main__":
    main()
