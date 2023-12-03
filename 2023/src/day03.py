from collections import defaultdict
from dataclasses import dataclass, field
from itertools import product
from pathlib import Path
from typing import Iterable, Iterator, overload

from loguru import logger
from typer import Typer

from utils import timer

main = Typer()


@dataclass
class Number:
    """Represent number with neighboring symbols"""

    value: int
    neighbors: set[str] = field(default_factory=set)


class Schematic:
    """Represent the schematic"""

    def __init__(self, input: str):
        self.grid = input.splitlines()

    @overload
    def __getitem__(self, key: tuple[int, int]) -> str:
        """Access via tuple"""
        ...

    @overload
    def __getitem__(self, row: int, col: int) -> str:
        """Access via row and col"""
        ...

    def __getitem__(self, *args):
        """Actual implementation"""
        if len(args) == 1:
            return self.grid[args[0][0]][args[0][1]]

        return self.grid[args[0]][args[1]]

    @property
    def n_rows(self) -> int:
        return len(self.grid)

    @property
    def n_cols(self) -> int:
        return len(self.grid[0])

    def is_symbol(
        self, row_or_tuple: int | tuple[int, int], col: int | None = None
    ) -> bool:
        """Check if coordinate is a symbol

        Digits and '.' are not symbols. Everything else is.

        Args:
            row_or_tuple: Either row of the schematic or a tuple with the
            schematic coordinates.
            col: Column go schematic. Ignored if row_or_tuple is a tuple. Defaults to None.

        Returns:
            True if symbol, False otherwise
        """
        if row_or_tuple is tuple:
            char = self[row_or_tuple]
        else:
            char = self[row_or_tuple, col]

        return not char.isdigit() and char != "."

    def get_symbol_neighbors(self, row: int, col: int) -> Iterator[tuple[int, int]]:
        """Get all neighbors that are symbols

        Takes diagonal neighbors into account. Respects schematic boundaries.

        Args:
            row: Current row
            col: Current column

        Yields:
            (row, col) of all neighbors that are symbols (not digits or '.'
        """
        n_rows = self.n_rows
        n_cols = self.n_cols

        for row_delta, col_delta in product([-1, 0, 1], [-1, 0, 1]):
            if row_delta == 0 and col_delta == 0:
                continue

            if (
                0 <= row + row_delta < n_rows
                and 0 <= col + col_delta < n_cols
                and self.is_symbol(row + row_delta, col + col_delta)
            ):
                yield row + row_delta, col + col_delta

    @timer
    def extract_numbers_and_engines(self) -> tuple[list[Number], list[Number]]:
        """Extracts the numbers and engines from the schematic

        Numbers also save the neighboring symbols. Engines is just a list of
        lists of numbers. Each list of numbers represents numbers neighboring a single engine.

        Returns:
            (numbers, engines)
        """
        numbers: list[Number] = []
        engines: dict[tuple[int, int], list[Number]] = defaultdict(list)

        for row in range(self.n_rows):
            # Reset number, neighbors, and engines
            value = 0
            neighbors: set[str] = set()
            is_number = False
            engine_set: set[tuple[int, int]] = set()

            # Scan row for numbers
            for col in range(self.n_cols):
                char = self[row, col]
                if char.isdigit():
                    value = 10 * value + int(char)
                    is_number = True

                    # Check neighbors for symbols
                    # TODO: Could be made more efficient by only checking
                    # the right 3 neighbors (except for the first column
                    # where we also need top and bottom)
                    for t in self.get_symbol_neighbors(row, col):
                        symbol = self[t]
                        neighbors.add(symbol)
                        if symbol == "*":
                            engine_set.add(t)

                    continue

                # If char is no digit, we have to check if we have a number
                # that has to be added to the list
                # Also add the number to the engine list, if it borders an engine
                if is_number:
                    numbers.append(Number(value, neighbors))
                    for t in engine_set:
                        engines[t].append(numbers[-1])

                value = 0
                neighbors = set()
                is_number = False
                engine_set = set()

            # Necessary if number is at the end of the row
            if is_number:
                numbers.append(Number(value, neighbors))
                for t in engine_set:
                    engines[t].append(numbers[-1])

        return numbers, list(engines.values())


@timer
def task01(numbers: list[Number]) -> int:
    """Sum all numbers that have neighbors"""
    return sum(number.value for number in numbers if len(number.neighbors) != 0)


@timer
def task02(engines: Iterable[list[Number]]) -> int:
    """Sum all gear ratios

    Only engines with exactly two numbers have a gear ratio, which
    is the product of the numbers.
    """
    return sum(
        numbers[0].value * numbers[1].value for numbers in engines if len(numbers) == 2
    )


@main.command()
def entrypoint(path: Path):
    """Entrypoint

    Args:
        path: Path to input file
    """
    with open(path, "r") as f:
        input = f.read().strip()

    schematic = Schematic(input)
    numbers, engines = schematic.extract_numbers_and_engines()

    logger.info("Task 01")
    logger.info(task01(numbers))
    logger.info("Task 02")
    logger.info(task02(engines))


if __name__ == "__main__":
    main()
