from dataclasses import dataclass
from pathlib import Path

from loguru import logger
from typer import Typer
from utils import timer

main = Typer()


@dataclass
class Coord:
    """Helper class for coordinates"""

    x: int
    y: int

    def __add__(self, other: "Coord") -> "Coord":
        return Coord(self.x + other.x, self.y + other.y)

    def __sub__(self, other: "Coord") -> "Coord":
        return Coord(self.x - other.x, self.y - other.y)

    def __abs__(self) -> int:
        return abs(self.x) + abs(self.y)


def empty_rows_cols(input: str) -> tuple[list[int], list[int]]:
    """Find empty rows and columns

    Args:
        input: Input string

    Returns:
        (empty row indices, empty column indices)
    """
    lines = input.splitlines()
    empyt_rows: list[int] = []
    for i, line in enumerate(lines):
        if line.strip() == "":
            continue
        if "#" not in line:
            empyt_rows.append(i)

    empty_cols: list[int] = []
    for j in range(len(lines[0])):
        if "#" not in [line[j] for line in lines]:
            empty_cols.append(j)

    return empyt_rows, empty_cols


def parse_input(input: str, expand: int = 2) -> list[Coord]:
    """Parse input to galaxy coordinates

    Args:
        input: Input string
        expand: Expansion of empty rows/cols. Defaults to 2.

    Returns:
        List with galaxy coordinates
    """
    empty_rows, empty_cols = empty_rows_cols(input)

    galaxies: list[Coord] = []
    row_idx = 0
    for i, line in enumerate(input.splitlines()):
        while row_idx < len(empty_rows) and empty_rows[row_idx] <= i:
            row_idx += 1

        col_idx = 0
        for j, c in enumerate(line):
            while col_idx < len(empty_cols) and empty_cols[col_idx] <= j:
                col_idx += 1

            if c == "#":
                galaxies.append(
                    Coord(i + (expand - 1) * row_idx, j + (expand - 1) * col_idx)
                )

    return galaxies


def sum_shortest_paths(galaxies: list[Coord]) -> int:
    """Sum of pairwise shortest paths between galaxies

    Distance is computed using L1 norm.

    Args:
        galaxies: list of galaxy coordinates

    Returns:
        Sum of shortes path lengths
    """
    res = 0
    for i, galaxy in enumerate(galaxies):
        for other_galaxy in galaxies[i + 1 :]:
            res += abs(galaxy - other_galaxy)

    return res


@timer
def tasks(input: str, expand: int = 2) -> int:
    """Task solutions

    Args:
        input: Input string
        expand: Expansion of empty rows/cols. Defaults to 2.

    Returns:
        Sum of shortest paths between galaxies
    """
    galaxies = parse_input(input=input, expand=expand)
    return sum_shortest_paths(galaxies=galaxies)


@main.command()
def entrypoint(path: Path):
    """CLI entrypoint"""
    with open(path, "r") as f:
        input = f.read().strip()

    logger.info(f"Task 01: {tasks(input)}")
    logger.info(f"Task 02: {tasks(input, expand=1_000_000)}")


if __name__ == "__main__":
    main()
