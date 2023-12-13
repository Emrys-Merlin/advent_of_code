from collections import Counter
from dataclasses import dataclass
from pathlib import Path

from loguru import logger
from typer import Typer
from utils import timer

main = Typer()


@dataclass
class Grid:
    grid: list[list[str]]

    @staticmethod
    def palindrome_offsets(line: list[str], seeds: list[int]) -> list[int]:
        """Check if the line is a palindrome at the given seeds
        Args:
            line: Line to check
            seeds: Seeds to check

        Returns:
            Reduced seeds that are still palindromes
        """
        reduced_seeds: list[int] = []
        for seed in seeds:
            for i in range(min(seed, len(line) - seed)):
                if line[seed - i - 1] != line[seed + i]:
                    break
            else:
                reduced_seeds.append(seed)

        return reduced_seeds

    def find_reflection(self) -> int:
        """Find the reflection row or column

        Returns:
            n_cols if reflection is a column or
            100 * n_rows if reflection is a row
        """
        seeds = list(range(1, len(self.grid[0])))
        for row in self.grid:
            seeds = self.palindrome_offsets(row, seeds)
            if len(seeds) == 0:
                break

        if len(seeds) == 1:
            return seeds[0]

        seeds = list(range(1, len(self.grid)))
        for col in zip(*self.grid):
            seeds = self.palindrome_offsets(col, seeds)

        return seeds[0] * 100

    def find_smudge_reflection(self) -> int:
        """Find reflections taking a smudge into account

        Smudge means one ./# can flip. Hence we check each
        row and column for a palindrome and use the index
        that works for all but one row/col.

        This is only a heuristic as we do not check if the
        missing row can be turned into a functional row by
        flipping a single ./#. In principle, the row/col
        could need more than one flip.

        Returns:
            n_cols if smudged refelction is a column or
            100 * n_rows if smudged reflection is a row
        """
        seeds = list(range(1, len(self.grid[0])))
        counter = Counter()
        for row in self.grid:
            new_seeds = self.palindrome_offsets(row, seeds)
            counter.update(new_seeds)

        for seed, count in counter.items():
            if count == len(self.grid) - 1:
                return seed

        seeds = list(range(1, len(self.grid)))
        counter = Counter()
        for col in zip(*self.grid):
            new_seeds = self.palindrome_offsets(col, seeds)
            counter.update(new_seeds)

        for seed, count in counter.items():
            if count == len(self.grid[0]) - 1:
                return seed * 100

    def __str__(self) -> str:
        return "\n".join(["".join(row) for row in self.grid])


@timer
def tasks(input: str) -> tuple[int, int]:
    """Parse and solve the tasks

    Args:
        input: Input string

    Returns:
        (Solution 1, Solution 2)
    """
    lines: list[list[str]] = []
    res = 0
    res2 = 0
    for line in input.splitlines():
        line = line.strip()
        if line == "":
            grid = Grid(lines)
            res += grid.find_reflection()
            res2 += grid.find_smudge_reflection()
            lines = []
            continue

        lines.append(list(line))

    grid = Grid(lines)
    res += grid.find_reflection()
    res2 += grid.find_smudge_reflection()

    return res, res2


@main.command()
def entrypoint(path: Path):
    """Entrypoint CLI"""
    with open(path, "r") as f:
        input = f.read().strip()

    res1, res2 = tasks(input)
    logger.info(f"Task 01: {res1}")
    logger.info(f"Task 02: {res2}")


if __name__ == "__main__":
    main()
