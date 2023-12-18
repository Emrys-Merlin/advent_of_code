from dataclasses import dataclass
from itertools import product
from pathlib import Path
from typing import Iterator, Literal

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
    direction: Literal["R", "L", "D", "U"]
    step_size: int
    color: str

    @classmethod
    def from_line(cls, line: str) -> "Instruction":
        direction, step_str, color = line.strip().split()
        return cls(direction, int(step_str), color[2:-1])

    def dig(
        self, start: tuple[int, int], alternative: bool = False
    ) -> Iterator[tuple[int, int]]:
        """Dig a straight line"""
        if alternative:
            direction = ALTERNATIVE_DIRECTIONS[int(self.color[-1])]
            step_size = int(self.color[:-1], base=16)
        else:
            direction = self.direction
            step_size = self.step_size
        delta_row, delta_col = DIRECTION[direction]

        for step in range(1, step_size + 1):
            row = start[0] + step * delta_row
            col = start[1] + step * delta_col
            yield (row, col)


class Grid:
    def __init__(self, input: str, alternative: bool = False):
        current = (0, 0)
        self.trench: set[tuple[int, int]] = {current}
        self.instructions: list[Instruction] = []
        top_left = (0, 0)
        bottom_right = (0, 0)
        for line in input.splitlines():
            instruction = Instruction.from_line(line)
            self.instructions.append(instruction)

            for coord in instruction.dig(current, alternative=alternative):
                self.trench.add(coord)
                top_left = (
                    min(top_left[0], coord[0]),
                    min(top_left[1], coord[1]),
                )
                bottom_right = (
                    max(bottom_right[0], coord[0]),
                    max(bottom_right[1], coord[1]),
                )

            current = coord

        self.top_left = top_left
        self.bottom_right = bottom_right
        self.fill()

    def _dfs(self, start: tuple[int, int]) -> set[tuple[int, int]]:
        stack = [start]
        visited: set[tuple[int, int]] = set()

        while stack:
            node = stack.pop()

            if (
                node in visited
                or node in self.trench
                or node[0] < self.top_left[0]
                or node[0] > self.bottom_right[0]
                or node[1] < self.top_left[1]
                or node[1] > self.bottom_right[1]
            ):
                continue

            visited.add(node)

            for delta_row, delta_col in product([-1, 0, 1], repeat=2):
                stack.append((node[0] + delta_row, node[1] + delta_col))

        return visited

    def fill(self):
        start = next(iter(self.trench))
        for delta_row, delta_col in product([-1, 0, 1], repeat=2):
            row = start[0] + delta_row
            col = start[1] + delta_col
            if (row, col) in self.trench:
                continue

            possible_fill = self._dfs((row, col))
            if all(
                self.top_left[0] < row < self.bottom_right[0]
                and self.top_left[1] < col < self.bottom_right[1]
                for row, col in possible_fill
            ):
                self.trench.update(possible_fill)
                return

        raise Exception("Could not fill")

    @property
    def width(self) -> int:
        return self.bottom_right[1] - self.top_left[1] + 1

    @property
    def height(self) -> int:
        return self.bottom_right[0] - self.top_left[0] + 1

    @property
    def trench_size(self) -> int:
        return len(self.trench)

    def __str__(self) -> str:
        return "\n".join(
            "".join(
                "#" if (row, col) in self.trench else "."
                for col in range(self.top_left[1], self.bottom_right[1] + 1)
            )
            for row in range(self.top_left[0], self.bottom_right[0] + 1)
        )


# 43734 too high
@timer
def task01(input: str) -> int:
    grid = Grid(input)
    return grid.trench_size


@timer
def task02(input: str) -> int:
    grid = Grid(input, alternative=True)
    return grid.trench_size


@main.command()
def entrypoint(path: Path):
    with open(path, "r") as f:
        input = f.read().strip()

    logger.info(f"Task01: {task01(input)}")
    # logger.info(f"Task02: {task02(input)}")


if __name__ == "__main__":
    main()
