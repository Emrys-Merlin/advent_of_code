from dataclasses import dataclass
from pathlib import Path
from typing import Optional

import click


@dataclass()
class Sediment:
    """The sediments that make up the cave

    Returns
    -------
    Sediment
        Position + if it's a rock or sand
    """

    x: int
    y: int
    rock: bool = True

    def __hash__(self) -> int:
        return hash((self.x, self.y))

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, Sediment):
            return NotImplemented

        return self.x == other.x and self.y == other.y


class Cave:
    def __init__(self, input: str, sand_entry: int = 500) -> None:
        """The Cave

        Parameters
        ----------
        input : str
            String representation with alle the rock formations
        sand_entry : int, optional
            Where the sand enters, by default 500

        Raises
        ------
        Exception
            Error if the lines are not all parallel
        """
        self.sand_entry = sand_entry

        self.sediments: set[Sediment] = set()

        self.xmin = int(1e9)
        self.xmax = int(-1e9)
        self.ymin = 0
        self.ymax = int(-1e9)

        for line in input.split("\n"):
            line = line.strip()
            if len(line) == 0:
                continue
            x_last: Optional[int] = None
            y_last: Optional[int] = None
            for coord in line.split("->"):
                parts = coord.strip().split(",")
                x, y = int(parts[0]), int(parts[1])
                if x_last is None or y_last is None:
                    x_last = x
                    y_last = y
                    continue

                if x == x_last:
                    start = min(y, y_last)
                    end = max(y, y_last)

                    if start < self.ymin:
                        self.ymin = start
                    if end > self.ymax:
                        self.ymax = end

                    if x < self.xmin:
                        self.xmin = x
                    elif x > self.xmax:
                        self.xmax = x

                    for i in range(start, end + 1):
                        self.sediments.add(
                            Sediment(
                                x=x,
                                y=i,
                            )
                        )
                elif y == y_last:
                    start = min(x, x_last)
                    end = max(x, x_last)

                    if start < self.xmin:
                        self.xmin = start
                    if end > self.xmax:
                        self.xmax = end

                    if y < self.ymin:
                        self.ymin = y
                    elif y > self.ymax:
                        self.ymax = y

                    for i in range(start, end + 1):
                        self.sediments.add(
                            Sediment(
                                x=i,
                                y=y,
                            )
                        )
                else:
                    raise Exception("Lines are not axis parallel!")

                x_last = x
                y_last = y

    def add_sand_corn(self, with_floor: bool = False) -> bool:
        """Add a single sand corn with or without floor considered

        Parameters
        ----------
        with_floor : bool, optional
            Assume floor at 2 + ymax, by default False

        Returns
        -------
        bool
            Without floor: False if sandcorn falls off into the void. Otherwise True
            With floor: False if sandcorn stops at sand entry point. Otherwise True
        """
        sand = Sediment(
            x=self.sand_entry,
            y=0,
            rock=False,
        )

        while with_floor or (
            self.xmin <= sand.x and self.xmax >= sand.x and self.ymax >= sand.y
        ):

            sand.y += 1
            if with_floor and sand.y == self.ymax + 2:
                sand.y -= 1
                self.sediments.add(sand)
                return True

            if sand not in self.sediments:
                continue
            sand.x -= 1
            if sand not in self.sediments:
                continue
            sand.x += 2
            if sand not in self.sediments:
                continue

            sand.y -= 1
            sand.x -= 1
            self.sediments.add(sand)
            if with_floor:
                return not (sand.y == 0 and sand.x == self.sand_entry)
            else:
                return True

        return False

    def n_stable_or_full(self, with_floor: bool = False) -> int:
        """Add multiple sandcorsn "until convergence"

        Parameters
        ----------
        with_floor : bool, optional
            Whether there is a floor at 2 + ymax, by default False

        Returns
        -------
        int
            Without floor: Number of sandcorns until they start to fall off
            With floor: Number of sandcorns until the first stops at the sand entry
        """
        i = 1 if with_floor else 0

        while self.add_sand_corn(with_floor=with_floor):
            # print(self)
            i += 1

        return i

    def reset(self):
        """Reset cave by removing all sandcorns"""
        self.sediments = set(sed for sed in self.sediments if sed.rock)

    def __str__(self) -> str:
        """Print cave for debugging

        Returns
        -------
        str
            String representation of cave
        """
        grid = [
            ["." for _ in range(self.xmax + 1 - self.xmin)]
            for _ in range(self.ymax + 1 - self.ymin)
        ]

        for sediment in self.sediments:
            y = sediment.y - self.ymin
            x = sediment.x - self.xmin
            if sediment.rock:
                grid[y][x] = "#"
            else:
                grid[y][x] = "o"

        grid[0][self.sand_entry - self.xmin] = "+"

        return "\n".join("".join(row) for row in grid)


@click.command()
@click.argument("fn", type=click.Path(exists=True, dir_okay=False))
def main(fn: str):
    """Solution for day 14

    FN should point to the input file for today.

    \f
    Parameters
    ----------
    fn : str
        Path to input file
    """

    in_fn = Path(fn)

    with open(in_fn, "r") as f:
        input = f.read()

    cave = Cave(input)

    # print(f"{cave.xmin=}")
    # print(f"{cave.xmax=}")
    # print(f"{cave.ymin=}")
    # print(f"{cave.ymax=}")

    # print(cave)

    res = cave.n_stable_or_full()

    print(f"Task 01: {res}")

    cave.reset()

    res = cave.n_stable_or_full(with_floor=True)
    print(f"Task 02: {res}")


if __name__ == "__main__":
    main()
