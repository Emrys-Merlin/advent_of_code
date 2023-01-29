from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path
from typing import Iterator

import click


@dataclass(frozen=True)
class Elf:
    """Elf

    Parameters
    -------
    row : int
        Row coordinate
    col : int
        Column coordinate
    """

    row: int
    col: int

    def __add__(self, other: "Elf") -> "Elf":
        return Elf(row=self.row + other.row, col=self.col + other.col)

    def neighbors(self, positions: set["Elf"]) -> Iterator["Elf"]:
        """Find existing neighbors of the elf.


        Yields
        ------
        Elf
            All neighbors directly next to the elf (taking diagonals into account)
        """
        for delta in [N, NE, E, SE, S, SW, W, NW]:
            neighbor = self + delta
            if neighbor in positions:
                yield neighbor

    def proposed_position(self, direction_counter: int, neighbors: set["Elf"]) -> "Elf":
        """In which direction to move, if a neighbor exists

        Parameters
        ----------
        direction_counter : int
            The direction which is considered first, circles with each round. That's
            encoded in the direction counter.
        neighbors : set[Elf]
            The set of neighbors of the elf

        Returns
        -------
        Elf
            Proposed position
        """
        for i in range(len(DIRECTION_ORDER)):
            idx = (i + direction_counter) % len(DIRECTION_ORDER)
            free, delta = DIRECTION_ORDER[idx]
            if neighbors.isdisjoint(self + free_pos for free_pos in free):
                return self + delta

        return self


N = Elf(row=-1, col=0)
NE = Elf(row=-1, col=1)
E = Elf(row=0, col=1)
SE = Elf(row=1, col=1)
S = Elf(row=1, col=0)
SW = Elf(row=1, col=-1)
W = Elf(row=0, col=-1)
NW = Elf(row=-1, col=-1)
DIRECTION_ORDER = [
    ([NW, N, NE], N),
    ([SE, S, SW], S),
    ([SW, W, NW], W),
    ([NE, E, SE], E),
]


class Grid:
    def __init__(self, grid: str):
        """Initialize grid with elves

        Parameters
        ----------
        grid : str
            String representation of grid
        """
        self.positions: set[Elf] = set()
        for row, line in enumerate(grid.split("\n")):
            line = line.strip()
            if len(line) == 0:
                continue
            for col, char in enumerate(line):
                if char == "#":
                    self.positions.add(Elf(row=row, col=col))

    def update(self, n_steps: int | None = None) -> int:
        """Update the grid with elves moving according to algorithm

        Parameters
        ----------
        n_steps : int | None, optional
            Maximum number of steps to consider, by default None

        Returns
        -------
        int
            Number of steps actually taken (either until convergence or max n steps)
        """

        direction_counter = 0
        while True:
            if n_steps is not None and direction_counter >= n_steps:
                break

            new_positions: set[Elf] = set()
            proposed_positions: dict[Elf, list[Elf]] = defaultdict(list)

            for elf in self.positions:
                neighbors = set(elf.neighbors(self.positions))
                if len(neighbors) == 0:
                    new_positions.add(elf)
                    continue

                proposed_pos = elf.proposed_position(
                    direction_counter=direction_counter, neighbors=neighbors
                )
                if elf == proposed_pos:
                    new_positions.add(elf)
                    continue

                proposed_positions[proposed_pos].append(elf)

            for new_pos, elves in proposed_positions.items():
                if len(elves) == 1:
                    new_positions.add(new_pos)
                else:
                    new_positions.update(elves)

            direction_counter += 1
            if self.positions == new_positions:
                break

            self.positions = new_positions

        return direction_counter

    def ground_tiles_in_rectangle(self) -> int:
        """Find minimal axis aligned rectangle containing all elves and compute ground tiles within.

        Returns
        -------
        int
            Ground tiles withing axis aligned minimal rectangle with all elves.
        """
        n_elves = len(self.positions)
        min_row = int(1e10)
        min_col = int(1e10)
        max_row = -int(1e10)
        max_col = -int(1e10)

        for elf in self.positions:
            if elf.row < min_row:
                min_row = elf.row

            if elf.row > max_row:
                max_row = elf.row

            if elf.col < min_col:
                min_col = elf.col

            if elf.col > max_col:
                max_col = elf.col

        return (max_row + 1 - min_row) * (max_col + 1 - min_col) - n_elves

    def __str__(self) -> str:
        min_row = int(1e10)
        min_col = int(1e10)
        max_row = -int(1e10)
        max_col = -int(1e10)

        for elf in self.positions:
            if elf.row < min_row:
                min_row = elf.row

            if elf.row > max_row:
                max_row = elf.row

            if elf.col < min_col:
                min_col = elf.col

            if elf.col > max_col:
                max_col = elf.col

        grid = [
            ["." for _ in range(min_col, max_col + 1)]
            for _ in range(min_row, max_row + 1)
        ]

        for elf in self.positions:
            row = elf.row - min_row
            col = elf.col - min_col
            grid[row][col] = "#"

        return "\n".join("".join(line) for line in grid)


@click.command()
@click.argument("fn", type=click.Path(exists=True, dir_okay=False))
def main(fn: str):
    """Solution for day 23

    FN should point to the input file for today.

    \f
    Parameters
    ----------
    fn : str
        Path to input file
    """
    in_fn = Path(fn)

    with open(in_fn, "r") as f:
        string_grid = f.read()

    grid = Grid(string_grid)
    grid.update(n_steps=10)
    ground_tiles = grid.ground_tiles_in_rectangle()
    print(f"Task01: {ground_tiles}")

    grid = Grid(string_grid)
    n_steps = grid.update()
    print(f"Task02: {n_steps}")


if __name__ == "__main__":
    main()
