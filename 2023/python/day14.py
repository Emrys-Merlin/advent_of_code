from dataclasses import dataclass
from pathlib import Path
from typing import Literal

from loguru import logger
from typer import Typer
from utils import timer

main = Typer()


@dataclass
class Grid:
    """Game board abstraction"""

    grid: list[list[str]]

    @classmethod
    def from_input(cls, input: str) -> "Grid":
        """Parse string input"""
        return cls([list(line.strip()) for line in input.splitlines()])

    @property
    def n_rows(self) -> int:
        return len(self.grid)

    @property
    def n_cols(self) -> int:
        return len(self.grid[0])

    @property
    def total_load(self) -> int:
        return sum(
            (self.n_rows - i) * sum(symbol == "O" for symbol in line)
            for i, line in enumerate(self.grid)
        )

    @staticmethod
    def move_line(line: list[str]):
        """Move rocks along a single line

        Operates inplace

        Args:
            line: List of ".", "#", and/or "O".
        """
        free_pos: int | None = None

        for i, symbol in enumerate(line):
            if symbol == "#":
                free_pos = None
                continue

            if symbol == ".":
                if free_pos is None:
                    free_pos = i
                continue

            # symbol == 'O'
            if free_pos is None:
                continue

            line[free_pos] = "O"
            line[i] = "."
            free_pos += 1

    def tilt(self, direction: Literal["N", "E", "S", "W"] = "N"):
        """Tilt the board in the given direction

        Args:
            direction: Either "N", "E", "S", or "W". Defaults to "N".
        """

        # TODO get rid of the whole transpose and double reverse stuff
        if direction == "W":
            for row in self.grid:
                self.move_line(row)

            return

        if direction == "E":
            for i, row in enumerate(self.grid):
                row.reverse()
                self.move_line(row)
                row.reverse()
                self.grid[i] = row

            return

        if direction == "N":
            transposed_grid = []
            for col in zip(*self.grid):
                col = list(col)
                self.move_line(col)
                transposed_grid.append(col)

            self.grid = list(list(row) for row in zip(*transposed_grid))

            return

        transposed_grid = []
        for col in zip(*self.grid):
            col = list(reversed(col))
            self.move_line(col)
            col.reverse()
            transposed_grid.append(col)

        self.grid = list(list(row) for row in zip(*transposed_grid))

    def spin_cycle(self):
        """Tilt the board in all four directions

        Counterclockwise. Starting from North.
        """
        self.tilt("N")
        self.tilt("W")
        self.tilt("S")
        self.tilt("E")

    def __str__(self) -> str:
        return "\n".join("".join(row) for row in self.grid)


@timer
def task01(input: str) -> int:
    """Solution for task01

    Args:
        input: Input string of the board

    Returns:
        Total load after tilt to North
    """
    grid = Grid.from_input(input)
    grid.tilt()
    return grid.total_load


@timer
def task02(input: str, n_cycles: int = 1_000_000_000) -> int:
    """Solution for task02

    Args:
        input: Input string of the board
        n_cycles: How often to spin the board. Defaults to 1_000_000_000.

    Returns:
        Total load after n_cycles
    """
    grid = Grid.from_input(input)

    state_dict: dict[str, int] = {str(grid): 0}
    lookup: list[str] = [str(grid)]
    for i in range(1, n_cycles + 1):
        grid.spin_cycle()

        if str(grid) in state_dict:
            offset = state_dict[str(grid)]
            cycle_length = i - offset
            delta = (n_cycles - offset) % cycle_length

            res_grid = Grid.from_input(lookup[delta + offset])
            return res_grid.total_load

        state_dict[str(grid)] = i
        lookup.append(str(grid))

    return grid.total_load


@main.command()
def entrypoint(path: Path):
    """Entrypoint CLI"""
    with open(path, "r") as f:
        input = f.read().strip()

    logger.info(f"Task 01: {task01(input)}")
    logger.info(f"Task 02: {task02(input)}")


if __name__ == "__main__":
    main()
