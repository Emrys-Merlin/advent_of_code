from copy import deepcopy
from dataclasses import dataclass, field
from pathlib import Path

import click


@dataclass(frozen=True)
class Coord:
    """2D integer coordinates with addition and subtraction"""

    x: int
    y: int

    def __add__(self, other: "Coord") -> "Coord":
        """Componentwise addition

        Parameters
        ----------
        other : Coord
            Other coordinate

        Returns
        -------
        Coord
            Componentwise sum of coordinates
        """
        return Coord(self.x + other.x, self.y + other.y)

    def __sub__(self, other: "Coord") -> "Coord":
        """Componentwise subtraction

        Parameters
        ----------
        other : Coord
            Other coordinate

        Returns
        -------
        Coord
            Componentwise difference of coordinates
        """
        return Coord(self.x - other.x, self.y - other.y)


@dataclass
class Shape:
    """Dropped shapes

    Parameters
    ----------
    locations : set[Coord]
        Set of coordinates describing the shape.
    """

    locations: set[Coord]
    # Shapes in drop order
    INITIAL_SHAPES = [
        set([Coord(0, 0), Coord(1, 0), Coord(2, 0), Coord(3, 0)]),  # -
        set([Coord(0, 1), Coord(1, 1), Coord(2, 1), Coord(1, 0), Coord(1, 2)]),  # +
        set(
            [Coord(0, 0), Coord(1, 0), Coord(2, 0), Coord(2, 1), Coord(2, 2)]
        ),  # mirrored L
        set([Coord(0, 0), Coord(0, 1), Coord(0, 2), Coord(0, 3)]),  # |
        set([Coord(0, 0), Coord(0, 1), Coord(1, 0), Coord(1, 1)]),  # square
    ]

    # Dictionary for how shapes might move
    DELTA = {
        ">": Coord(1, 0),
        "<": Coord(-1, 0),
        "v": Coord(0, -1),
    }

    @classmethod
    def spawn(cls, initial_pos: Coord, idx: int) -> "Shape":
        """Spawn a new shape referenced by index at given position

        Parameters
        ----------
        initial_pos : Coord
            Coordinate describing the leftern and lower edge of the shape
        idx : int
            Index to choose shape. Will be interpreted mod len(INITIAL_SHAPES)

        Returns
        -------
        Shape
            The shape at the given initial position
        """
        idx %= len(cls.INITIAL_SHAPES)
        return Shape(set(coord + initial_pos for coord in cls.INITIAL_SHAPES[idx]))

    def next_pos(self, direction: str) -> "Shape":
        """Move shape one step

        Parameters
        ----------
        direction : str
            Has to be either, "<", ">", or "v", describing movement to the left, right, or down.

        Returns
        -------
        Shape
            Shape at the new position
        """
        delta = self.DELTA[direction]

        return Shape(set(coord + delta for coord in self.locations))

    @property
    def height(self) -> int:
        """Returns the height of the shape

        Used mainly for printing and debugging.

        Returns
        -------
        int
            Height of the shape
        """
        return (
            1
            + max(coord.y for coord in self.locations)
            - min(coord.y for coord in self.locations)
        )


@dataclass
class Grid:
    """Representation of the cave grid

    Parameters
    -------
    jets : str
        String representation of the jets in the cave
    width : int
        Width of the cave, by default 7
    height : int
        Height to which the cave is already filled, by defaul 0
    filled_locations : set[Coord]
        Which locations are already occupied due to a shape, by default empty.
    active_shape : Shape | None
        The currently dropped shaped. Mostly used for printing and debugging.
    """

    jets: str
    width: int = 7
    height: int = 0
    filled_locations: set[Coord] = field(default_factory=set)
    active_shape: Shape | None = None

    def collision(self, shape: Shape) -> bool:
        """Check if shape collides with a filled location or a boundary

        Parameters
        ----------
        shape : Shape
            Shape to test for collision

        Returns
        -------
        bool
            True, if collision is detected
        """
        return (not self.filled_locations.isdisjoint(shape.locations)) or any(
            (coord.x < 0 or coord.y < 0 or coord.x >= 7) for coord in shape.locations
        )

    def drop_shapes(self, n_shapes: int, jet_idx: int = 0) -> tuple[int, int]:
        """Naive implementation of sequential shape dropping

        Sequentially the shapes are spawned and then moved according to the rules.
        If a collision during downwards movement is detected, the shape is fixed and
        the next shape is spawned.

        Parameters
        ----------
        n_shapes : int
            How many shapes to drop
        jet_idx : int, optional
            Index at which to start with the jets, by default 0

        Returns
        -------
        tuple[int, int]
            Tuple with height of the tower and the ned jet_idx
        """
        for i in range(n_shapes):
            spawn_pos = Coord(2, self.height + 3)

            self.active_shape = Shape.spawn(spawn_pos, i)

            horizontal = True
            while True:
                if not horizontal:
                    direction = "v"
                else:
                    direction = self.jets[jet_idx]
                    jet_idx = (jet_idx + 1) % len(self.jets)

                new_shape = self.active_shape.next_pos(direction=direction)

                if not self.collision(new_shape):
                    self.active_shape = new_shape
                else:
                    if not horizontal:
                        self.filled_locations.update(self.active_shape.locations)
                        self.height = max(
                            1 + max(coord.y for coord in self.active_shape.locations),
                            self.height,
                        )
                        break

                horizontal = not horizontal

        self.active_shape = None
        return self.height, jet_idx

    @staticmethod
    def remove_y_offset(locations: set[Coord]) -> set[Coord]:
        """Set minimum y coord of a set of coords to 0

        Parameters
        ----------
        locations : set[Coord]
            Set of coordinates

        Returns
        -------
        set[Coord]
            Set of coordinates with min y coord translated to 0
        """
        y_offset = min(coord.y for coord in locations)
        offset = Coord(0, y_offset)
        return set(coord - offset for coord in locations)

    def check_cycle(
        self, cycle_offset: int, cycle_length: int, cycle_height: int, jet_idx: int
    ) -> bool:
        """Check if the provided parameters describe a valid cycle.

        Because of the small width we can assume that the not many shapes can pass by
        previous shapes. So it stands to reason that after some time we find a repeating
        drop pattern. This pattern is described by an offset after which the pattern begins
        the length of the cycle, the height added by the cycle and at what jet index we end up.

        In this method we test whether the provided parameters constitue a cycle. The method assumes
        that currently cycle_offset + cycle_length shapes have been dropped. The method adds another
        cycle_length of shapes. Then it tests, if we end up at the same jet_idx again and if the new added
        shapes add cycle_height. If this is indeed the case, we analyze the cycle candidate in more detail.
        For this, we extract the locations filled from cycle_length + cycle_length to cycle_length + 2cycle_length
        and compare this shape to the shape of the locations filled from cycle_length to cycle_lengt + cycle_length.
        If they coincide (up to a y offset), then the parameters form a cycle.

        Parameters
        ----------
        cycle_offset : int
            Offset before the cycle start
        cycle_length : int
            Length (in number of shapes dropped) of the cycle
        cycle_height : int
            Height added by the cycle
        jet_idx : int
            Jet index after the cycle

        Returns
        -------
        bool
            True, if the parameters constitute a cycle
        """
        current_filled_locations = deepcopy(self.filled_locations)
        current_height = self.height

        new_height, new_jet_idx = self.drop_shapes(
            n_shapes=cycle_length, jet_idx=jet_idx
        )

        new_cycle_height = new_height - current_height
        new_jet_idx %= len(self.jets)
        new_filled_locations = deepcopy(self.filled_locations)

        self.filled_locations = current_filled_locations
        self.height = current_height

        if new_jet_idx != jet_idx or new_cycle_height != cycle_height:
            return False

        new_cycle_shape = self.remove_y_offset(
            new_filled_locations.difference(current_filled_locations)
        )

        tmp_grid = Grid(self.jets)
        tmp_grid.drop_shapes(n_shapes=cycle_offset)
        cycle_shape = self.remove_y_offset(
            current_filled_locations.difference(tmp_grid.filled_locations)
        )

        return cycle_shape == new_cycle_shape

    def find_cycle(self) -> tuple[int, int, int]:
        """Find a cycle in the drop pattern

        We try to detect a cycle by dropping a full sequence of shapes
        (5 in our case) and checking whether we end up at a jet index
        that we have seen before. If yes, we extract the corresponding
        cycle_offset, cycle_height, and cycle_length and check if this
        candidate actually is a cycle.

        Note for a full description of a cycle we would need to return
        jet_idx too. However, our implementation does not require it.

        Returns
        -------
        tuple[int, int, int]
            cycle_offset, cycle_legnth, cycle_height
        """
        jet_idx = 0
        n_shapes = len(Shape.INITIAL_SHAPES)
        idx_height_dict: dict[int, tuple[int, int]] = {jet_idx: (0, 0)}

        length = 0
        while True:
            height, jet_idx = self.drop_shapes(n_shapes=n_shapes, jet_idx=jet_idx)
            length += n_shapes
            jet_idx %= len(self.jets)
            if jet_idx in idx_height_dict:
                cycle_offset, height_offset = idx_height_dict[jet_idx]

                cycle_height = height - height_offset
                cycle_length = length - cycle_offset

                if self.check_cycle(
                    cycle_offset=cycle_offset,
                    cycle_length=cycle_length,
                    cycle_height=cycle_height,
                    jet_idx=jet_idx,
                ):
                    break
                else:
                    continue

            idx_height_dict[jet_idx] = (length, height)

        (
            cycle_offset,
            height_offset,
        ) = idx_height_dict[jet_idx]
        cycle_length = length - cycle_offset
        cycle_height = height - height_offset

        return cycle_offset, cycle_length, cycle_height

    def drop_shapes_smarter(self, n_shapes: int) -> int:
        """Find the height of the tower by finding a cycle

        First, we find a cycle in the drop pattern. Then we
        reduce the amount of computation by only considering the
        remainder of n_shapes with regard to the cycle_length
        (taking the cycle_offset into account).

        Parameters
        ----------
        n_shapes : int
            Number of shapes to drop

        Returns
        -------
        int
            Height of the tower
        """
        cycle_offset, cycle_length, cycle_height = self.find_cycle()
        self.reset()
        n_missing = n_shapes - cycle_offset

        if n_missing < 0:
            return self.drop_shapes(n_shapes)[0]

        n_cycles = n_missing // cycle_length
        remainder = n_missing % cycle_length

        height, _ = self.drop_shapes(n_shapes=cycle_offset + remainder)

        return n_cycles * cycle_height + height

    def reset(self):
        """Reset the grid

        Drop filled locations and reset height to 0. Drop active shape.
        """
        self.height = 0
        self.filled_locations = set()
        self.active_shape = None

    def __str__(self) -> str:
        """String representation for grid

        Mostly used for debuggin.

        Returns
        -------
        str
            String representation of grid.
        """
        if self.active_shape is not None:
            height_offset = 3 + self.active_shape.height
        else:
            height_offset = 4
        grid = [
            ["." for _ in range(self.width)] for _ in range(self.height + height_offset)
        ]

        for coord in self.filled_locations:
            grid[coord.y][coord.x] = "#"

        if self.active_shape is not None:
            for coord in self.active_shape.locations:
                grid[coord.y][coord.x] = "@"

        return (
            "\n".join("|" + "".join(row) + "|" for row in reversed(grid))
            + "\n+"
            + "-" * self.width
            + "+"
        )


@click.command()
@click.argument("fn", type=click.Path(exists=True, dir_okay=False))
def main(fn: str):
    """Solution for day 17

    FN should point to the input file for today.

    \f
    Parameters
    ----------
    fn : str
        Path to input file
    """
    in_fn = Path(fn)

    with open(in_fn, "r") as f:
        jets = f.readline().strip()

    grid = Grid(jets)

    height, _ = grid.drop_shapes(n_shapes=2022)
    print(f"Task01: {height}")

    grid.reset()

    height = grid.drop_shapes_smarter(n_shapes=1_000_000_000_000)
    print(f"Task02: {height}")


if __name__ == "__main__":
    main()
