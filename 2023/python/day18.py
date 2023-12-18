from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, Literal

from loguru import logger
from typer import Typer
from utils import timer

main = Typer()

DIRECTION: dict[str, tuple[int, int]] = {
    "R": (0, 1),
    "L": (0, -1),
    "U": (-1, 0),
    "D": (1, 0),
}

ALTERNATIVE_DIRECTIONS = list("RDLU")


@dataclass
class Instruction:
    """Instruction parser"""

    direction: Literal["R", "L", "D", "U"]
    step_size: int
    color: str

    @classmethod
    def from_line(cls, line: str) -> "Instruction":
        direction, step_str, color = line.strip().split()
        return cls(direction, int(step_str), color[2:-1])

    def get_direction_step_size(self, alternative: bool = False) -> tuple[str, int]:
        """Depending on the alternative, return the direction and step size

        Args:
            alternative: If False, simply return direction and step_size. Otherwise,
            parse the hex number to get the values. Defaults to False.

        Returns:
            (direction("R", "L", "D", "U"), step_size(int))
        """
        if alternative:
            direction = ALTERNATIVE_DIRECTIONS[int(self.color[-1])]
            step_size = int(self.color[:-1], base=16)
        else:
            direction = self.direction
            step_size = self.step_size

        return direction, step_size

    def new_corner(
        self, start: tuple[int, int], alternative: bool = False
    ) -> tuple[int, int]:
        """Return the new corner of the polygon following the instructions

        Args:
            start: Start point
            alternative: How to parse instructions. Defaults to False.

        Returns:
            New corner
        """
        direction, step_size = self.get_direction_step_size(alternative=alternative)
        delta_row, delta_col = DIRECTION[direction]

        return (start[0] + step_size * delta_row, start[1] + step_size * delta_col)


def shoelace_formula(points: Iterable[tuple[int, int]]) -> int:
    """Closed formula for the area of a polygon based on the coordinates of its vertices

    Args:
        points: Vertices (or corners) of the polygon

    Returns:
        Area of the polygon
    """
    x, y = zip(*points)
    return (
        abs(sum(x[i] * y[i + 1] - x[i + 1] * y[i] for i in range(-1, len(x) - 1))) // 2
    )


def area_with_boundary(input: str, alternative: bool = False) -> int:
    """Area of the polygon

    Based on Pick's theorem. The area of a polygon whose vertices are grid points is
    A = i + b/2 - 1, where i is the number of interior points and b is the number of
    boundary points.
    As the "area" we are looking for is the number of points in the polygon (including the
    boundary), we can rewrite the formula as i + b = A + 1 + b/2, where we get A from
    the shoelace formula and b by counting the boundary points.

    Thanks to https://github.com/fspoettel, who showed me this approach.

    Args:
        input: Input instructions
        alternative: How the instructions are parsed. False, for task01, True, for task02.
        Defaults to False.

    Returns:
        Area of the polygon (in the sense that all points in the polygon (including boundary) are counted)
    """

    border = 0
    points: list[tuple[int, int]] = [(0, 0)]
    for line in input.splitlines():
        instruction = Instruction.from_line(line)
        border += instruction.get_direction_step_size(alternative=alternative)[1]
        points.append(instruction.new_corner(points[-1], alternative=alternative))

    return shoelace_formula(points) + 1 + border // 2


@timer
def task01(input: str) -> int:
    """Solution to task01

    Args:
        input: Input instructions as string

    Returns:
        Area
    """
    return area_with_boundary(input)


@timer
def task02(input: str) -> int:
    """Solution to task02

    Args:
        input: Input instructions as string

    Returns:
        Area
    """
    return area_with_boundary(input, alternative=True)


@main.command()
def entrypoint(path: Path):
    """CLI entrypoint"""
    with open(path, "r") as f:
        input = f.read().strip()

    logger.info(f"Task01: {task01(input)}")
    logger.info(f"Task02: {task02(input)}")


if __name__ == "__main__":
    main()
