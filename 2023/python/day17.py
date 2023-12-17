from dataclasses import dataclass, field
from heapq import heappop, heappush
from pathlib import Path
from typing import Iterator, Optional

from loguru import logger
from typer import Typer
from utils import timer

main = Typer()

TURNS: dict[tuple[int, int], list[tuple[int, int]]] = {
    (0, 1): [(1, 0), (-1, 0)],
    (0, -1): [(1, 0), (-1, 0)],
    (1, 0): [(0, 1), (0, -1)],
    (-1, 0): [(0, 1), (0, -1)],
}


@dataclass
class Crucible:
    row: int = 0
    col: int = 0
    delta_row: int = 0
    delta_col: int = 0
    heat_loss: int = 0
    straight_counter: int = 0
    previous: Optional["Crucible"] = field(default=None, repr=False)

    def __lt__(self, other: "Crucible") -> bool:
        return self.heat_loss < other.heat_loss

    @property
    def coord(self) -> tuple[int, int]:
        return (self.row, self.col)

    @property
    def delta_coord(self) -> tuple[int, int]:
        return (self.delta_row, self.delta_col)

    @property
    def state(self) -> tuple[int, int, int, int, int]:
        return self.row, self.col, self.delta_row, self.delta_col, self.straight_counter

    def neighbors(
        self, grid: "Grid", min_straight: int = 0, max_straight: int = 3
    ) -> Iterator["Crucible"]:
        """Get admissible neighboring crucibles

        Args:
            grid: Grid with heat loss values
            min_straight: Minimum number of straight steps before a turn
                can be taken.
            max_straight: Maximum number of straight steps before a turn
                must be taken.

        Yields:
            Admissible neighboring crucibles
        """
        deltas_straight: list[tuple[int, int, int]] = []

        if self.straight_counter < max_straight:
            deltas_straight.append((*self.delta_coord, self.straight_counter))

        if self.straight_counter >= min_straight:
            deltas_straight.extend((*delta, 0) for delta in TURNS[self.delta_coord])

        for delta_row, delta_col, straight_counter in deltas_straight:
            row = self.row + delta_row
            col = self.col + delta_col

            if 0 <= row < grid.height and 0 <= col < grid.width:
                yield Crucible(
                    row=row,
                    col=col,
                    delta_row=delta_row,
                    delta_col=delta_col,
                    heat_loss=self.heat_loss + grid[row, col],
                    straight_counter=straight_counter + 1,
                    previous=self,
                )


@dataclass
class Grid:
    grid: list[list[int]]

    @classmethod
    def from_input(cls, input: str) -> "Grid":
        return cls(grid=[[int(c) for c in line] for line in input.split("\n")])

    @property
    def height(self) -> int:
        return len(self.grid)

    @property
    def width(self) -> int:
        return len(self.grid[0])

    def __getitem__(self, coord: tuple[int, int]) -> int:
        row, col = coord
        return self.grid[row][col]

    def minimum_heat_loss(self, ultra: bool = False) -> int:
        """Compute path with minimum heat loss

        Implemented via dijkstra's algorithm

        Args:
            ultra: If true, use ultra crucibles, else normal crucibles. Defaults to False.

        Returns:
            Total heat loss
        """
        heap = [Crucible(delta_col=0, delta_row=1), Crucible(delta_col=1, delta_row=0)]
        visited: set[tuple[int, int, int, int, int]] = set()

        if ultra:
            min_straight = 4
            max_straight = 10
        else:
            min_straight = 0
            max_straight = 3

        while heap:
            crucible = heappop(heap)
            if crucible.state in visited:
                continue

            visited.add(crucible.state)

            if crucible.coord == (self.height - 1, self.width - 1) and (
                crucible.straight_counter >= min_straight
            ):
                return crucible.heat_loss

            for neighbor in crucible.neighbors(
                self, min_straight=min_straight, max_straight=max_straight
            ):
                heappush(heap, neighbor)

    def visualize_path(self, crucible: Crucible) -> str:
        """Visualize the path

        For debugging purposes

        Args:
            crucible: End of the path

        Returns:
            String representation of grid with inscribed path
        """
        path: set[tuple[int, int]] = set()
        heat_loss = 0
        counter = 0
        while crucible is not None:
            path.add(crucible.coord)
            heat_loss += self[*crucible.coord]
            counter += 1
            crucible = crucible.previous

        return (
            "\n".join(
                "".join(
                    "." if (row, col) in path else str(self[row, col])
                    for col in range(self.width)
                )
                for row in range(self.height)
            )
            + "\nHeat loss: "
            + str(heat_loss)
            + "\nCounter: "
            + str(counter)
        )


@timer
def task01(input: str) -> int:
    """Solution for task 01

    Args:
        input: Grid string representation

    Returns:
        Total heat loss for normal crucibles
    """
    grid = Grid.from_input(input)
    return grid.minimum_heat_loss()


@timer
def task02(input: str) -> int:
    """Solution for task 02

    Args:
        input: Grid string representation

    Returns:
        Total heat loss for ultra crucibles
    """
    grid = Grid.from_input(input)
    return grid.minimum_heat_loss(ultra=True)


@main.command()
def entrypoint(path: Path):
    with open(path, "r") as f:
        input = f.read().strip()

    logger.info(f"Task01: {task01(input)}")
    logger.info(f"Task02: {task02(input)}")


if __name__ == "__main__":
    main()
