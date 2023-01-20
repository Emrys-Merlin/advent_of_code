import re
from dataclasses import dataclass
from pathlib import Path
from typing import Iterator

import click


@dataclass(frozen=True)
class Coord:
    """Simple coordinate class

    Returns
    -------
    Coord
        2D coordinate with addition, subtraction, and L1-norm
    """

    x: int
    y: int

    def __add__(self, other: "Coord") -> "Coord":
        """Add two coordiantes

        Parameters
        ----------
        other : Coord
            Other coordinate

        Returns
        -------
        Coord
            Componentwise addition
        """
        return Coord(self.x + other.x, self.y + other.y)

    def __sub__(self, other: "Coord") -> "Coord":
        """Subtract two coordiantes

        Parameters
        ----------
        other : Coord
            Other coordinate

        Returns
        -------
        Coord
            Componentwise subtraction
        """
        return Coord(self.x - other.x, self.y - other.y)

    def __abs__(self) -> int:
        """Compute L1-norm of coordinate

        Returns
        -------
        int
            L1-norm of (integer) coordinates
        """
        return abs(self.x) + abs(self.y)


@dataclass(frozen=True)
class Patch:
    """Class representing a rectangular patch.

    Please note that all boundaries are inclusive, e.g. the patch
    Patch(x0=1, x1=1, y0=3, y1=3) consists of a single point, namely
    (1, 3), as opposed to being empty.
    """

    x0: int
    x1: int
    y0: int
    y1: int

    def subdivide(self) -> list["Patch"]:
        """Subdivide the current patch into four (almost) equally sized subpatches

        Subdivision might return less than four subpatches, if no more subdivision
        is possible along an axis.

        Returns
        -------
        list[Patch]
            List of subpatches
        """
        x_mid = (self.x0 + self.x1) // 2
        y_mid = (self.y0 + self.y1) // 2

        res = [Patch(self.x0, x_mid, self.y0, y_mid)]

        if x_mid < self.x1:
            res.append(Patch(x_mid + 1, self.x1, self.y0, y_mid))
        if y_mid < self.y1:
            res.append(Patch(self.x0, x_mid, y_mid + 1, self.y1))
        if len(res) == 3:
            res.append(Patch(x_mid + 1, self.x1, y_mid + 1, self.y1))

        return res

    def corners(self) -> Iterator[Coord]:
        """Return the four corner points as coordinates of the patch

        Yields
        ------
        Iterator[Coord]
            The four corner points of the patch
        """
        for x in [self.x0, self.x1]:
            for y in [self.y0, self.y1]:
                yield Coord(x, y)

    def __len__(self) -> int:
        """Return size of the patch

        Returns
        -------
        int
            Size of the patch
        """
        return (self.x1 - self.x0 + 1) * (self.y1 - self.y0 + 1)


class Sensor:
    def __init__(self, sensor_coord: Coord, beacon_coord: Coord) -> None:
        """Construct sensor

        Sensor location and beacon location is saved and the radius of the
        sensor is computed.

        Parameters
        ----------
        sensor_coord : Coord
            Sensor location
        beacon_coord : Coord
            Beacon location
        """
        self.sensor = sensor_coord
        self.beacon = beacon_coord
        self.radius = abs(self.beacon - self.sensor)

    @staticmethod
    def from_line(line: str) -> "Sensor":
        """Construct sensor from string input line

        Parameters
        ----------
        line : str
            Input line

        Returns
        -------
        Sensor
            Sensor representation
        """
        match = re.match(
            pattern=r"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)",
            string=line,
        )

        assert match is not None

        sensor = Coord(int(match.group(1)), int(match.group(2)))
        beacon = Coord(int(match.group(3)), int(match.group(4)))
        return Sensor(sensor, beacon)

    def boundaries(self) -> tuple[int, int, int, int]:
        """For printing the grid, return the minimum and maximum coordinates covered
        by this sensor beacon pair

        Returns
        -------
        tuple[int, int, int, int]
            x_min, x_max, y_min, y_max
        """
        return (
            min(self.sensor.x, self.beacon.x),
            max(self.sensor.x, self.beacon.x),
            min(self.sensor.y, self.beacon.y),
            max(self.sensor.y, self.beacon.y),
        )

    def covers_patch(self, patch: Patch) -> bool:
        """Check whether a patch is completely covered by the radius of the sensor

        For that we only need to check that the for corner points of the patch are
        within the radius.

        Parameters
        ----------
        patch : Patch
            Patch to check

        Returns
        -------
        bool
            True, if patch is covered by the sensor
        """
        return all(abs(coord - self.sensor) <= self.radius for coord in patch.corners())


class Grid:
    def __init__(self, input: str) -> None:
        """Initialize sensor beacon grid

        Parameters
        ----------
        input : str
            String input representation
        """
        self.sensors: list[Sensor] = []
        self.minx = int(1e10)
        self.maxx = -int(1e10)
        self.miny = int(1e10)
        self.maxy = -int(1e10)

        for line in input.split("\n"):
            line = line.strip()
            if len(line) == 0:
                continue

            sensor = Sensor.from_line(line)
            xl, xh, yl, yh = sensor.boundaries()
            if xl < self.minx:
                self.minx = xl
            if xh > self.maxx:
                self.maxx = xh
            if yl < self.miny:
                self.miny = yl
            if yh > self.maxy:
                self.maxy = yh

            self.sensors.append(sensor)

    def __str__(self) -> str:
        """String representation of grid for debugging

        Returns
        -------
        str
            String representation of grid
        """
        grid = [
            ["." for _ in range(self.maxx + 1 - self.minx)]
            for _ in range(self.maxy + 1 - self.miny)
        ]
        for sensor in self.sensors:
            s = sensor.sensor
            grid[s.y - self.miny][s.x - self.minx] = "S"
            b = sensor.beacon
            grid[b.y - self.miny][b.x - self.minx] = "B"

        return "\n".join("".join(line) for line in grid)

    def n_pos_covered(self, row: int) -> int:
        """Check the number of coordinates covered by the sensors for row

        Parameters
        ----------
        row : int
            Which row to count

        Returns
        -------
        int
            Number of covered coordinates
        """
        covered_pos: set[int] = set()
        for sensor in self.sensors:
            pos_x = sensor.sensor.x
            coord = Coord(pos_x, row)

            remainder = sensor.radius - abs(sensor.sensor - coord)
            if remainder >= 0:
                for i in range(remainder + 1):
                    covered_pos.add(pos_x - i)
                    covered_pos.add(pos_x + i)

        for sensor in self.sensors:
            beacon = sensor.beacon
            if beacon.y != row:
                continue
            if beacon.x in covered_pos:
                covered_pos.remove(beacon.x)

        return len(covered_pos)

    def tuning_frequency(self, hi_bound: int = 4_000_000) -> int:
        """Find the one coordinate not covered in the patch and compute tuning frequency

        This method implements a divide an conquer approach. We start with the whole patch
        and check whether it is covered by a single sensor. If not, we subdivide the patch
        in (up to) four sub-patches and check each individually. We do this iteratively until
        we find a sub-patch of size 1 that is not covered.

        Parameters
        ----------
        hi_bound : int, optional
            Upper limit of the initial patch, by default 4_000_000

        Returns
        -------
        int
            tuning frequency (equivalently position) of the missing beacon coordinate
        """
        patch = Patch(0, hi_bound, 0, hi_bound)

        stack = [patch]

        while len(stack) != 0:
            patch = stack.pop()

            if any(sensor.covers_patch(patch) for sensor in self.sensors):
                continue

            if len(patch) == 1:
                return 4_000_000 * patch.x0 + patch.y0

            stack.extend(patch.subdivide())

        return -1


@click.command()
@click.argument("fn", type=click.Path(exists=True, dir_okay=False))
@click.option(
    "--example/--no-example",
    default=False,
    help="Switch the row and boundary to the example case.",
)
def main(fn: str, example: bool = False):
    """Solution for day 15

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

    grid = Grid(input)

    if example:
        row = 10
        hi_bound = 20
    else:
        row = 2_000_000
        hi_bound = 4_000_000

    n_covered = grid.n_pos_covered(row)
    print(f"Task01: {n_covered}")

    frequency = grid.tuning_frequency(hi_bound)
    print(f"Task2: {frequency}")


if __name__ == "__main__":
    main()
