from dataclasses import dataclass
from pathlib import Path
from typing import Iterator

import click


@dataclass(frozen=True)
class Cube:
    """Abstraction for 1x1x1 Cube

    Parameters
    -------
    x : int
        x-coord
    y : int
        y-coord
    z : int
        z-coord
    """

    x: int
    y: int
    z: int

    def __add__(self, other: "Cube") -> "Cube":
        """Add cubes componentwisely

        Parameters
        ----------
        other : Cube
            Other cube

        Returns
        -------
        Cube
            Componentwise sum
        """
        return Cube(self.x + other.x, self.y + other.y, self.z + other.z)

    def __sub__(self, other: "Cube") -> "Cube":
        """Subtract cubes componentwisely

        Parameters
        ----------
        other : Cube
            Other cube

        Returns
        -------
        Cube
            Componentwise difference
        """
        return Cube(self.x - other.x, self.y - other.y, self.z - other.z)

    def neighbors(self) -> Iterator["Cube"]:
        """Generate the neighboring cubes

        Yields
        ------
        Cube
            The cubes neighboring one facet of the cube
        """
        for i in range(3):
            for sign in [1, -1]:
                coords = [0] * 3
                coords[i] = sign
                delta = Cube(*coords)
                yield self + delta

    def __lt__(self, other: "Cube") -> bool:
        """Lower than comparison

        Does not really constitue a total order (I think). Used to detect
        cubes that are too far away from the droplet.

        Checks if any component is lower than the corresponding component
        of the other cube.

        Parameters
        ----------
        other : Cube
            Other cube

        Returns
        -------
        bool
            True, if any component of self is lower than the corresponding
            component of other.
        """
        return any(getattr(self - other, axis) < 0 for axis in ["x", "y", "z"])

    def __gt__(self, other: "Cube") -> bool:
        """Greater than comparison

        Does not really constitue a total order (I think). Used to detect
        cubes that are too far away from the droplet.

        Checks if any component is greater than the corresponding component
        of the other cube.

        Parameters
        ----------
        other : Cube
            Other cube

        Returns
        -------
        bool
            True, if any component of self is greater than the corresponding
            component of other.
        """
        return any(getattr(self - other, axis) > 0 for axis in ["x", "y", "z"])


class Droplet:
    def __init__(self, input: str):
        """Instantiate droplet from input

        Each cube is recorded and assumed to have 6 free surfaces.
        Then all neighbors are checked. If one is part of the droplet
        the number for free surfaces for both cubes is reduced by 1.

        For the second task, upper and lower bounds for the cube coordinates
        are determined and saved in bounding cubes.

        Parameters
        ----------
        input : str
            Text input to the task
        """
        self.free_surfaces: dict[Cube, int] = {}
        lower_bounds = [int(1e10)] * 3
        upper_bounds = [-int(1e10)] * 3

        for line in input.split("\n"):
            line = line.strip()
            if len(line) == 0:
                continue

            coords = [int(c) for c in line.split(",")]
            cube = Cube(*coords)
            for i in range(3):
                if coords[i] < lower_bounds[i]:
                    lower_bounds[i] = coords[i]
                if coords[i] > upper_bounds[i]:
                    upper_bounds[i] = coords[i]

            self.free_surfaces[cube] = 6

            for neighbor in cube.neighbors():
                if neighbor in self.free_surfaces:
                    self.free_surfaces[neighbor] -= 1
                    self.free_surfaces[cube] -= 1

        self.lower_cube = Cube(*[n - 1 for n in lower_bounds])
        self.upper_cube = Cube(*[n + 1 for n in upper_bounds])

    @property
    def n_facets(self) -> int:
        """Return number of facets of the droplet

        Returns
        -------
        int
            Number of facets
        """
        return sum(self.free_surfaces.values())

    def out_of_bounds(self, cube: Cube) -> bool:
        """Check if cube is too far away from the droplet

        Parameters
        ----------
        cube : Cube
            Cube to check for out of bounds

        Returns
        -------
        bool
            True, if the any of the cubes dimension is higher or lower
            than the largest or lowest droplet dimension +1 or -1 respectively.
        """
        return cube < self.lower_cube or cube > self.upper_cube

    def n_reachable_facets(self) -> int:
        """Compute the number of reachable facets

        Implements a depth first search (DFS) on a cube that
        contains the complete droplet with a margin of 1 to the closest
        droplet-cube surface. The search starts with a point guaranteed
        to be outside the droplet and the search cannot pass through
        droplet-cubes. Whenever the search brings up a droplet cube,
        a reachable facet is found and the count is increased.
        After the DFS finishes, we have found all reachable facets.

        Returns
        -------
        int
            Number of reachable facets
        """
        n_reachable_facets = 0
        start = self.lower_cube
        stack = [start]
        visited: set[Cube] = set()

        while len(stack) != 0:
            cube = stack.pop()

            if cube in visited:
                continue

            visited.add(cube)

            for neighbor in cube.neighbors():
                if self.out_of_bounds(neighbor):
                    continue

                if neighbor in self.free_surfaces:
                    n_reachable_facets += 1
                    continue

                stack.append(neighbor)

        return n_reachable_facets


@click.command()
@click.argument("fn", type=click.Path(exists=True, dir_okay=False))
def main(fn: str):
    """Solution for day 18

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

    droplet = Droplet(input)
    n_facets = droplet.n_facets
    print(f"Task01: {n_facets}")

    n_reachable_facets = droplet.n_reachable_facets()
    print(f"Task02: {n_reachable_facets}")


if __name__ == "__main__":
    main()
