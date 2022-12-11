from dataclasses import dataclass
from pathlib import Path

import click


@dataclass(frozen=True)
class Coordinate:
    """Rudimentary 2d vector

    Returns
    -------
    Coordinate
        2d vector with coordinates saved in x and y
    """

    x: int = 0
    y: int = 0

    def __add__(self, other: "Coordinate") -> "Coordinate":
        """Add two coordinates

        Parameters
        ----------
        other : Coordinate
            Other coordinate

        Returns
        -------
        Coordinate
            Componentwise addition
        """
        return Coordinate(
            x=self.x + other.x,
            y=self.y + other.y,
        )

    def __sub__(self, other: "Coordinate") -> "Coordinate":
        """Subtract two coordinates

        Parameters
        ----------
        other : Coordinate
            Other coordinate

        Returns
        -------
        Coordinate
            Componentwise subtraction
        """
        return Coordinate(
            x=self.x - other.x,
            y=self.y - other.y,
        )

    def max_norm(self) -> int:
        """Compute maximum norm of the coordinate

        Returns
        -------
        int
            Maxim norm
        """
        return max(abs(self.x), abs(self.y))

    def clip(self, bound: int = 1) -> "Coordinate":
        """Clip a coordinate

        Clip the coordinate ranges between [-bound, bound]
        for each component

        Parameters
        ----------
        bound : int, optional
            Clipping bound, by default 1

        Returns
        -------
        Coordinate
            Clipped coordiante
        """
        return Coordinate(
            x=max(-bound, min(bound, self.x)),
            y=max(-bound, min(bound, self.y)),
        )


class Grid:
    """Grid for the rope

    Returns
    -------
    Grid
        The grid with rope and visited tail locations
    """

    # Dict for head knot movement
    DELTA = {
        "R": Coordinate(1, 0),
        "L": Coordinate(-1, 0),
        "U": Coordinate(0, 1),
        "D": Coordinate(0, -1),
    }

    def __init__(self, n_knots: int = 2) -> None:
        """Init grid

        Parameters
        ----------
        n_knots : int, optional
            Number of knots in the rope, by default 2
        """
        self.knots = [Coordinate() for _ in range(n_knots)]
        self.tail_visited = set([self.knots[-1]])

    def move_rope(
        self,
        direction: str,
        step: int,
    ):
        """Move rope a single instruction

        Parameters
        ----------
        direction : str
            Direction to move. Must be in 'R', 'L', 'U', 'D'
        step : int
            How many steps to go in direction
        """
        delta = self.DELTA[direction]
        for _ in range(step):
            self.knots[0] += delta

            for i in range(1, len(self.knots)):
                diff = self.knots[i - 1] - self.knots[i]
                if diff.max_norm() <= 1:
                    continue

                self.knots[i] += diff.clip()

            self.tail_visited.add(self.knots[-1])

    def move_rope_multiple(
        self,
        directions: list[str],
        steps: list[int],
    ):
        """Move rope following multiple instructions

        Parameters
        ----------
        direction : str
            Directions to move. Each direction must be in 'R', 'L', 'U', 'D'
        step : int
            How many steps to go in corresponding direction
        """
        assert len(directions) == len(steps)
        for direction, step in zip(directions, steps):
            self.move_rope(direction, step)

    def n_tail_visited(self) -> int:
        """Return the number of grid fields visited by the tail

        Returns
        -------
        int
            Number of fields visited by tail knot
        """
        return len(self.tail_visited)

    def __str__(self) -> str:
        """Print grid for debugging purposes

        The knots are represented by letters a, b, c,...
        from head to tail. If the field of the knot was visited by the tail
        the letter is upper case. Otherwise, it is lower case.

        Returns
        -------
        str
            Strin representation of grid
        """
        all_coords = set(self.tail_visited).union(self.knots)
        x_min = min(coord.x for coord in all_coords)
        x_max = max(coord.x for coord in all_coords)
        y_min = min(coord.y for coord in all_coords)
        y_max = max(coord.y for coord in all_coords)

        grid = [
            ["." for _ in range(x_max + 1 - x_min)] for _ in range(y_max + 1 - y_min)
        ]

        for coord in self.tail_visited:
            grid[coord.y - y_min][coord.x - x_min] = "#"

        for i, knot in enumerate(self.knots):
            if grid[knot.y - y_min][knot.x - x_min] == "#":
                char = chr(i + ord("A"))
            else:
                char = chr(i + ord("a"))

            grid[knot.y - y_min][knot.x - x_min] = char

        return "\n".join("".join(row) for row in reversed(grid))


@click.command()
@click.argument("fn", type=click.Path(exists=True, dir_okay=False))
def main(fn: str):
    """Solution for day 09

    FN should point to the input file for today.

    \f
    Parameters
    ----------
    fn : str
        Path to input file
    """
    in_fn = Path(fn)

    directions: list[str] = []
    steps: list[int] = []
    with open(in_fn, "r") as f:
        for line in f.readlines():
            line = line.strip()
            if len(line) == 0:
                continue

            parts = line.split(" ")
            directions.append(parts[0])
            steps.append(int(parts[1]))

    grid = Grid()

    grid.move_rope_multiple(directions, steps)

    # print(grid)
    print(f"Task 01: {grid.n_tail_visited()}")

    grid2 = Grid(n_knots=10)
    grid2.move_rope_multiple(directions, steps)
    print(f"Task 02: {grid2.n_tail_visited()}")


if __name__ == "__main__":
    main()
