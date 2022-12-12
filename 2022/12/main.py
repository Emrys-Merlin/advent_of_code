from heapq import heappop, heappush
from pathlib import Path
from typing import Optional

import click


class Grid:
    def __init__(self, grid: str) -> None:
        """Initialize height grid

        All letters will be replaced by ord(letter)

        Parameters
        ----------
        grid : str
            Text representation of height grid
        """
        self.grid: list[list[int]] = []

        for i, line in enumerate(grid.split("\n")):
            line = line.strip()
            if len(line) == 0:
                continue

            row: list[int] = []
            for j, c in enumerate(line):
                if c == "S":
                    self.start = (i, j)
                    c = "a"
                elif c == "E":
                    self.end = (i, j)
                    c = "z"

                row.append(ord(c))

            self.grid.append(row)

    def shortest_path(
        self,
        start: Optional[tuple[int, int]] = None,
        max_dist: Optional[int | float] = None,
    ) -> int:
        """Compute shortest path from start to end

        Parameters
        ----------
        start : Optional[tuple[int, int]], optional
            If None, use the startign point encoded in text representation of grid,
            otherwise use the provided starting point, by default None
        max_dist : Optional[int  |  float], optional
            Only consider distances, up to max_dist. If the path becomes, longer return -1, by default None

        Returns
        -------
        int
            Either length of shortest path, or -1 if the path would be longer than max_dist (or no path is found)
        """
        if start is None:
            start = self.start

        visited: set[tuple[int, int]] = set([start])

        heap = [(0, start)]

        while len(heap) != 0:
            dist, pos = heappop(heap)

            if max_dist is not None and dist > max_dist:
                return -1

            if pos == self.end:
                return dist

            i, j = pos

            for delta in [-1, 1]:
                di, dj = (i + delta, j)
                next_pos = (di, dj)
                if (
                    0 <= di
                    and di < len(self.grid)
                    and self.grid[i][j] + 1 >= self.grid[di][dj]
                    and next_pos not in visited
                ):
                    heappush(heap, (dist + 1, next_pos))
                    visited.add(next_pos)

                di, dj = (i, j + delta)
                next_pos = (di, dj)
                if (
                    0 <= dj
                    and dj < len(self.grid[0])
                    and self.grid[i][j] + 1 >= self.grid[di][dj]
                    and next_pos not in visited
                ):
                    heappush(heap, (dist + 1, next_pos))
                    visited.add(next_pos)

        return -1

    def shortest_shortest_path(self) -> int:
        """Find the shortest path to the end position considering all locations with height 'a' as starting positions

        Returns
        -------
        int
            Minimum Distance to the end position (-1 if none is found)
        """
        minimal_height = ord("a")
        min_dist = len(self.grid) * len(self.grid[0])

        for i, row in enumerate(self.grid):
            for j, height in enumerate(row):
                if height == minimal_height:
                    dist = self.shortest_path(start=(i, j), max_dist=min_dist)
                    if dist != -1 and dist < min_dist:
                        min_dist = dist

        return min_dist

    def __str__(self) -> str:
        """Text representation for debugging

        Returns
        -------
        str
            Text representation of grid
        """
        grid_str = [[chr(height) for height in row] for row in self.grid]
        grid_str[self.start[0]][self.start[1]] = "S"
        grid_str[self.end[0]][self.end[1]] = "E"

        return "\n".join("".join(row) for row in grid_str)


@click.command()
@click.argument("fn", type=click.Path(exists=True, dir_okay=False))
def main(fn: str):
    """Solution for day 12

    FN should point to the input file for today.

    \f
    Parameters
    ----------
    fn : str
        Path to input file
    """
    in_fn = Path(fn)

    with open(in_fn, "r") as f:
        grid = Grid(f.read())

    dist = grid.shortest_path()
    print(f"Task 01: {dist}")

    min_dist = grid.shortest_shortest_path()
    print(f"Task 02: {min_dist}")


if __name__ == "__main__":
    main()
