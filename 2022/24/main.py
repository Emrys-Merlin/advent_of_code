from dataclasses import dataclass, field
from heapq import heappop, heappush
from math import lcm
from pathlib import Path
from typing import Iterator

import click


@dataclass(frozen=True)
class Coord:
    """Coordinates + time

    Parameters
    -------
    row : int
        Row index
    col : int
        Column index
    time : int
        Time step
    """

    row: int
    col: int
    time: int = 0

    def __add__(self, other: "Coord") -> "Coord":
        return Coord(
            row=self.row + other.row,
            col=self.col + other.col,
            time=self.time + other.time,
        )

    def __sub__(self, other: "Coord") -> "Coord":
        return Coord(
            row=self.row - other.row,
            col=self.col - other.col,
            time=self.time - other.time,
        )

    def __lt__(self, other: "Coord") -> bool:
        return self.time < other.time

    def next_coords(self, n_rows: int, n_cols: int) -> Iterator["Coord"]:
        """Generate the possible next coordinates taking the boundary into account

        Parameters
        ----------
        n_rows : int
            Number of rows
        n_cols : int
            Number of columsn

        Yields
        ------
        Coord
            Possible next coordinates
        """
        for delta in [
            Coord(0, 0, 1),
            Coord(0, 1, 1),
            Coord(0, -1, 1),
            Coord(1, 0, 1),
            Coord(-1, 0, 1),
        ]:
            neighbor = self + delta
            if (
                neighbor.row < 0
                or neighbor.row >= n_rows
                or neighbor.col < 0
                or neighbor.col >= n_cols
            ):
                continue

            yield neighbor

    def __mod__(self, n: int) -> "Coord":
        return Coord(row=self.row, col=self.col, time=self.time % n)


@dataclass
class BlizzardSchedule:
    """Global blizzard schedule

    Can be used to check if there is a blizzard at a given
    coordinate (spatial + time point).

    Parameters
    ----------
    period : int
        Period after which all blizzards are back at the starting position.
        (Is given by the least common multiple of n_row and n_col)
    schedule : set[Coord]
        All spatial+time coordinates at which a blizzard occurs
    """

    period: int
    schedule: set[Coord] = field(default_factory=set)

    def add(self, blizzard: Coord):
        """Add a coordinate to the blizzard schedule

        Parameters
        ----------
        blizzard : Coord
            Spatial coordinate + time at which blizzard occurs
        """
        self.schedule.add(blizzard % self.period)

    def blizzard(self, coord: Coord) -> bool:
        """Check for blizzard

        Checks whether a blizzard at the given spatial coordinate
        + time point occurs.

        Parameters
        ----------
        coord : Coord
            Coordinate to check

        Returns
        -------
        bool
            True, if a blizzard occurs
        """
        return (coord % self.period) in self.schedule


class Grid:
    DIRECTION_DICT: dict[str, Coord] = {
        ">": Coord(row=0, col=1, time=1),
        "<": Coord(row=0, col=-1, time=1),
        "^": Coord(row=-1, col=0, time=1),
        "v": Coord(row=1, col=0, time=1),
    }

    def __init__(self, grid: str) -> None:
        """Initialize grid

        The grid is read and based on the dimensions of the grid,
        the blizzards are transformed to a blizzard schedule for
        easy look up.

        Parameters
        ----------
        grid : str
            String representation of grid
        """
        lines = grid.split("\n")
        self.n_rows = len(lines) - 3
        self.n_cols = len(lines[0]) - 2
        assert self.n_rows > 0
        assert self.n_cols > 0
        self.period = lcm(self.n_rows, self.n_cols)
        self.schedule = BlizzardSchedule(self.period)

        for row, line in enumerate(lines, -1):
            line = line.strip()
            if len(line) == 0 or row < 0 or row >= self.n_rows:
                continue

            for col, c in enumerate(line, -1):
                if c == "." or col < 0 or col >= self.n_cols:
                    continue

                self.add2schedule(Coord(row, col, 0), c)

    def add2schedule(self, coord: Coord, c: str):
        """Add the blizzard at coord (with direction) to schedule

        The blizzard is moved for period time steps and the corresponding
        spatiotemporal coordinates are saved in the blizzard schedule

        Parameters
        ----------
        coord : Coord
            Initial coordinate with t=0 of the blizzard
        c : str
            Direction of the blizzard
        """
        delta = self.DIRECTION_DICT[c]

        while coord.time < self.period:
            self.schedule.add(coord)
            coord += delta
            if coord.row == self.n_rows:
                coord -= Coord(row=self.n_rows, col=0)
            elif coord.row == -1:
                coord += Coord(row=self.n_rows, col=0)
            elif coord.col == self.n_cols:
                coord -= Coord(row=0, col=self.n_cols)
            elif coord.col == -1:
                coord += Coord(row=0, col=self.n_cols)

    def at_end(self, coord: Coord, reverse: bool = False) -> bool:
        """Have we reached the upper left or lower right corner of the grid

        Parameters
        ----------
        coord : Coord
            Coord to check
        reverse : bool, optional
            True, means we look for upper left, False for lower right, by default False

        Returns
        -------
        bool
            True, if we have reached the correct point
        """
        if not reverse:
            return (coord.row == self.n_rows - 1) and (coord.col == self.n_cols - 1)
        return (coord.row == 0) and (coord.col == 0)

    def shortest_path(
        self, start: Coord = Coord(0, 0, 0), reverse: bool = False
    ) -> Coord:
        """Calculate shortest path from start coordinate

        Parameters
        ----------
        start : Coord, optional
            Initial coordinate, by default Coord(0, 0, 0)
        reverse : bool, optional
            If we go forward (to lower right, false) or backward (to upper left, true), by default False

        Returns
        -------
        Coord
            Final coordinate with time encoding path length
        """
        heap: list[Coord] = []
        for t in range(1, self.period + 1):
            actual = start + Coord(0, 0, t)
            if not self.schedule.blizzard(actual):
                heappush(heap, actual)

        visited: set[Coord] = set(heap)

        while len(heap) != 0:
            coord = heappop(heap)

            if self.at_end(coord=coord, reverse=reverse):
                return coord + Coord(0, 0, 1)

            for neighbor in coord.next_coords(self.n_rows, self.n_cols):
                if neighbor in visited or self.schedule.blizzard(neighbor):
                    continue
                visited.add(neighbor)
                heap.append(neighbor)

        return Coord(-1, -1, -1)

    def multiple_shortest_paths(self, n_times: int) -> Coord:
        """Compute multiple shortest paths, if we need to go back and forth

        Parameters
        ----------
        n_times : int
            How mane paths? (3 times means: one forward, one backward, one forward)

        Returns
        -------
        Coord
            Final coordinate with time encoding path lengths
        """
        start = Coord(0, 0, 0)
        for i in range(n_times):
            start = self.shortest_path(start, reverse=(i % 2 != 0))

        return start


@click.command()
@click.argument("fn", type=click.Path(exists=True, dir_okay=False))
def main(fn: str):
    """Solution for day 24

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
    final_coord = grid.shortest_path()
    path_length = final_coord.time
    print(f"Task01: {path_length}")

    final_coord = grid.multiple_shortest_paths(n_times=3)
    path_length = final_coord.time
    print(f"Task02: {path_length}")


if __name__ == "__main__":
    main()
