import re
from dataclasses import dataclass
from pathlib import Path

from loguru import logger
from typer import Typer
from utils import timer

main = Typer()


@dataclass
class MaxDraw:
    """Represent the maximum number of cubes drawn per game"""

    red: int
    green: int
    blue: int
    id: int = -1

    def __le__(self, other: "MaxDraw") -> bool:
        """self <= other iff it has less cubes then other for all colors"""
        return all(
            getattr(self, color) <= getattr(other, color)
            for color in ["red", "green", "blue"]
        )

    @classmethod
    def from_line(cls, line: str) -> "MaxDraw":
        """Parse a single line."""
        game, rest = line.split(":")
        id = int(game.split(" ")[-1])

        splits = re.split(r"\W+", rest.strip())

        counts: dict[str, int] = {}
        for number, color in zip(splits[::2], splits[1::2]):
            counts[color] = max(counts.get(color, 0), int(number))

        return cls(id=id, **counts)

    @property
    def power(self) -> int:
        """Compute the power by multiplying all the cube counts"""
        return self.red * self.green * self.blue


@timer
def sum_valid_games(input: str, ref: MaxDraw | None = None) -> int:
    """Compute the sum of the game ids of valid games

    A game is valid if it has less cubes than the reference game.
    If ref is None, the reference from the advent of code task is
    inserted.

    Args:
        input: Input string
        ref: Reference Draw representing the number of available cubes. Defaults to None.

    Returns:
        Sum of valid game ids
    """
    if ref is None:
        ref = MaxDraw(red=12, green=13, blue=14)

    res = 0
    for line in input.splitlines():
        line = line.strip()
        if len(line) == 0:
            continue

        draw = MaxDraw.from_line(line)
        if draw <= ref:
            res += draw.id

    return res


@timer
def total_power(input: str) -> int:
    """Compute the power of all games

    Args:
        input: String input

    Returns:
        Sum of power
    """
    return sum(
        MaxDraw.from_line(line.strip()).power
        for line in input.split("\n")
        if len(line.strip()) != 0
    )


@main.command()
def entrypoint(path: Path):
    """Entrypoint

    Args:
        path: Path to input file
    """
    with open(path, "r") as f:
        input = f.read()

    logger.info("Task 01")
    logger.info(sum_valid_games(input))

    logger.info("Task02")
    logger.info(total_power(input))


if __name__ == "__main__":
    main()
