from dataclasses import dataclass, field
from math import sqrt
from pathlib import Path
from typing import Iterator, Optional

import click


@dataclass(frozen=True)
class Coord:
    """Coordinates

    Parameters
    ----------
    row : int
        row index
    cols : int
        col index
    """

    row: int
    col: int

    def __add__(self, other: "Coord") -> "Coord":
        return Coord(row=self.row + other.row, col=self.col + other.col)

    def __sub__(self, other: "Coord") -> "Coord":
        return Coord(row=self.row - other.row, col=self.col - other.col)

    def __mul__(self, other: int) -> "Coord":
        return Coord(row=self.row * other, col=self.col * other)

    def __rmul__(self, other: int) -> "Coord":
        return Coord(row=self.row * other, col=self.col * other)


@dataclass()
class Node:
    """Single field on the map

    Parameters
    ----------
    coord : Coord
        Global coordinates with regard to the input map
    up, right, left, down : Node | None
        Neighbors taking the gluing instructions into accoutn
    blocked : bool
        True, if impassable field
    other_side : dict[str, str]
        Only used for the second task. Remembers with which side (up, right, left down) the current
        side corresponds on the other Node. Important for direction changes.
    """

    coord: Coord
    up: Optional["Node"] = None
    down: Optional["Node"] = None
    left: Optional["Node"] = None
    right: Optional["Node"] = None
    blocked: bool = False
    other_side: dict[str, str] = field(default_factory=dict)

    # For password computation
    HEADING_VALUE = {"right": 0, "down": 1, "left": 2, "up": 3}
    # The other_side correspondence has to be mirrored to get the right
    # continued direction
    MIRROR = {"up": "down", "right": "left", "down": "up", "left": "right"}

    def walk(self, direction: str, steps: int) -> tuple["Node", str]:
        """Walk steps in given direction

        Crossing over to a new facet (on the cube, task02) might
        change the initial direction. Because of this the direction
        is returned together with the node.

        Returns
        -------
        tuple[Node, str]
            final node and heading
        """
        node = self

        for _ in range(steps):
            next_node: Node = getattr(node, direction)

            if next_node.blocked:
                break

            # The Task01 implementation does not use the other_side
            # dict because all facets stay aligned to the global directions and the
            # dict can be omitted. This is caught with the if clause
            if direction in node.other_side:
                direction = self.MIRROR[node.other_side[direction]]

            node = next_node

        return node, direction

    def password(self, direction: str) -> int:
        """Compute password (Task solution)

        The pasword depends on row, col, and direction.

        Parameters
        ----------
        direction : str
            Final direction of the walker

        Returns
        -------
        int
            Password
        """
        row = self.coord.row
        col = self.coord.col
        return 1000 * (row + 1) + 4 * (col + 1) + self.HEADING_VALUE[direction]

    def neighbors_assigned(self) -> bool:
        """Check that this node has all neighbors assigned to it.

        I.e. we check that no up, right, down, left is None.

        Returns
        -------
        bool
            True, if all neighbors are assigned
        """
        return all(
            getattr(self, direction) is not None
            for direction in ["up", "right", "down", "left"]
        )

    def __str__(self) -> str:
        coord = self.coord
        up = self.up.coord if self.up is not None else None
        right = self.right.coord if self.right is not None else None
        down = self.down.coord if self.down is not None else None
        left = self.left.coord if self.left is not None else None
        blocked = self.blocked
        return f"Node({coord=}, {up=}, {right=}, {down=}, {left=}, {blocked=})"


class Map:
    """Base class for Task01 and Task 02

    Parameters
    ----------
    start : Node
        Start node for the paths
    """

    start: Node

    # Neighbors in global coordinate space
    LEFT = Coord(col=-1, row=0)
    RIGHT = Coord(col=1, row=0)
    UP = Coord(col=0, row=-1)
    DOWN = Coord(col=0, row=1)
    # (current direction, rotation) -> new direction
    ROTATE: dict[tuple[str, str], str] = {
        ("up", "R"): "right",
        ("right", "R"): "down",
        ("down", "R"): "left",
        ("left", "R"): "up",
        ("up", "L"): "left",
        ("left", "L"): "down",
        ("down", "L"): "right",
        ("right", "L"): "up",
    }

    @staticmethod
    def parse_instruction(instructions: str) -> Iterator[int | str]:
        """Parse instructions

        Separate the numbers from the rotation instructions and
        yield each.

        Parameters
        ----------
        instructions : str
            Instructions as read from input line

        Yields
        ------
        Iterator[int | str]
            int for number of steps, 'R' or 'L' for 90° rotation
        """
        number = 0
        for char in instructions:
            if char.isdigit():
                number = 10 * number + int(char)
            else:
                yield number
                yield char
                number = 0

        yield number

    def follow_path(self, instructions: str) -> int:
        """Follow the path given by the instructions

        Parameters
        ----------
        instructions : str
            Instructions as read from input line

        Returns
        -------
        int
            Password (Task solution)
        """
        node = self.start
        direction = "right"
        for instr in self.parse_instruction(instructions):
            if isinstance(instr, int):
                node, direction = node.walk(direction=direction, steps=instr)
                continue

            direction = self.ROTATE[(direction, instr)]

        return node.password(direction=direction)


class Maze(Map):
    """Task01 board"""

    def __init__(self, maze_lines: list[str]):
        """Initialize Task01 Map

        Parameters
        ----------
        maze_lines : list[str]
            The lines of the map of the input file
        """
        beginning = True
        grid: dict[Coord, Node] = {}
        uppermost_nodes: dict[int, Node] = {}
        lowermost_nodes: dict[int, Node] = {}
        for row, line in enumerate(maze_lines):
            for col, char in enumerate(line):
                if char not in [".", "#"]:
                    continue

                coord = Coord(row=row, col=col)
                node = Node(coord=coord, blocked=(char == "#"))
                grid[coord] = node

                left_neighbor = coord + self.LEFT
                if left_neighbor in grid:
                    grid[left_neighbor].right = node
                    node.left = grid[left_neighbor]
                else:
                    leftmost = node

                up_neighbor = coord + self.UP
                if up_neighbor in grid:
                    grid[up_neighbor].down = node
                    node.up = grid[up_neighbor]
                else:
                    uppermost_nodes[col] = node

                lowermost_nodes[col] = node

                if beginning:
                    self.start = node
                    beginning = False

            node.right = leftmost
            leftmost.left = node

        assert len(uppermost_nodes) == len(lowermost_nodes)

        for col, uppermost in uppermost_nodes.items():
            lowermost = lowermost_nodes[col]
            uppermost.up = lowermost
            lowermost.down = uppermost

        assert all(node.neighbors_assigned() for node in grid.values())


class Cube(Map):
    """Task02 Map"""

    def __init__(self, maze_lines: list[str]):
        """Initialize Task02 Map

        This is a four step process.
        1. The facets of the cube are identified and a smaller
            cube model is built with one Node per facet.
        2. On the smaller cube the correct gluing instructions are
            inferred.
        3. The nodes of the larger cubes are initialized but gluing
            is ignored.
        4. The gluing instructions from the small cube are transferred
            on the small group taking the orientation of the edges into
            account.

        Step 1 and 2 are performed in Cube.identify_gluing, step 3 is part
        of Cube.__init__, and step 4 is performed in Cube.glue_cube.

        Parameters
        ----------
        maze_lines : list[str]
            The lines of the map of the input file
        """
        super().__init__()
        facet_length, cube_grid = self.identify_cube(maze_lines=maze_lines)

        grid: dict[Coord, Node] = {}
        beginning = True
        for row, line in enumerate(maze_lines):
            for col, char in enumerate(line):
                if char not in [".", "#"]:
                    continue

                coord = Coord(row=row, col=col)
                node = Node(coord, blocked=(char == "#"))
                if beginning:
                    self.start = node
                    beginning = False

                grid[coord] = node

                left_neighbor = coord + self.LEFT
                if left_neighbor in grid:
                    node.left = grid[left_neighbor]
                    node.other_side["left"] = "right"
                    grid[left_neighbor].right = node
                    grid[left_neighbor].other_side["right"] = "left"

                up_neighbor = coord + self.UP
                if up_neighbor in grid:
                    node.up = grid[up_neighbor]
                    node.other_side["up"] = "down"
                    grid[up_neighbor].down = node
                    grid[up_neighbor].other_side["down"] = "up"

        self.glue_cube(grid, cube_grid, facet_length)

        assert all(node.neighbors_assigned() for node in grid.values())

    @classmethod
    def identify_cube(cls, maze_lines: list[str]) -> tuple[int, dict[Coord, Node]]:
        """Infer correct gluing of the facets

        Build a small cube with one node per facet. Then whenever
        there is a facet with two neighboring facets in a 90° distance (like up and right or
        down and left), then the two neighbors have to be glued together as well.
        This can be used iteratively to find the correct correspondences for all facet sides.

        Parameters
        ----------
        maze_lines : list[str]
            The lines of the map of the input file

        Returns
        -------
        tuple[int, dict[Coord, Node]]
            Length of a single facet (for the large cube), Representation of the small glued cube
        """
        n_total = sum(char in [".", "#"] for line in maze_lines for char in line)
        facet_length = int(sqrt(n_total // 6))
        assert n_total == 6 * facet_length**2

        # Populate facets
        cube_grid: dict[Coord, Node] = {}
        for row in range(0, len(maze_lines), facet_length):
            line = maze_lines[row]
            lo = min(line.find("."), line.find("#"))
            hi = max(line.rfind("."), line.rfind("#"))
            for col in range(lo, hi, facet_length):
                coord = Coord(row=row // facet_length, col=col // facet_length)
                node = Node(coord=coord)
                cube_grid[coord] = node

                left_neighbor = coord + cls.LEFT
                if left_neighbor in cube_grid:
                    node.left = cube_grid[left_neighbor]
                    node.other_side["left"] = "right"
                    cube_grid[left_neighbor].right = node
                    cube_grid[left_neighbor].other_side["right"] = "left"

                up_neighbor = coord + cls.UP
                if up_neighbor in cube_grid:
                    node.up = cube_grid[up_neighbor]
                    node.other_side["up"] = "down"
                    cube_grid[up_neighbor].down = node
                    cube_grid[up_neighbor].other_side["down"] = "up"

        # Glue facets
        while not all(node.neighbors_assigned() for node in cube_grid.values()):
            for node in cube_grid.values():
                for first, second in zip(
                    ["up", "right", "down", "left"], ["right", "down", "left", "up"]
                ):
                    neighbor1: Node | None = getattr(node, first)
                    neighbor2: Node | None = getattr(node, second)
                    if neighbor1 is None or neighbor2 is None:
                        continue

                    side1 = node.other_side[first]
                    side2 = node.other_side[second]

                    side1 = cls.ROTATE[(side1, "L")]
                    side2 = cls.ROTATE[(side2, "R")]

                    if (
                        getattr(neighbor1, side1) is not None
                        or getattr(neighbor2, side2) is not None
                    ):
                        continue

                    setattr(neighbor1, side1, neighbor2)
                    neighbor1.other_side[side1] = side2
                    setattr(neighbor2, side2, neighbor1)
                    neighbor2.other_side[side2] = side1

        return facet_length, cube_grid

    @staticmethod
    def glue_cube(
        grid: dict[Coord, Node], cube_grid: dict[Coord, Node], facet_length: int
    ):
        """Glue the larger cube

        A cube is an orientable surface, which implies that choosing a direction
        along which to run along the boundary of a facet (e.g. up -> right -> down -> left -> up),
        the shared boundary of neighboring facets are transferred in opposit directions by the two
        facet orientations. That's important to correctly glue the facets together.

        Basically, this method first enumerates all facets, then all sides per facet. If a node at the
        boundary of the side is not yet assigned. The correct other facet and side is determined and taking
        the orientation into account all nodes on the boundaries are glued together.

        The direction of the two facets are saved in the Node.other_side dict.

        Parameters
        ----------
        grid : dict[Coord, Node]
            Coordinate access to the larger cube nodes
        cube_grid : dict[Coord, Node]
            The small cube with the gluing instructions
        facet_length : int
            Length of a facet
        """
        # Side -> (Startpoint, Delta for traversal 'in orientation')
        # e.g. 'up' starts in the upper left corner and with successive
        # deltas ends up in the upper right corner.
        side_orientation: dict[str, tuple[Coord, Coord]] = {
            "up": (Coord(row=0, col=0), Coord(row=0, col=1)),
            "right": (Coord(row=0, col=facet_length - 1), Coord(row=1, col=0)),
            "down": (
                Coord(row=facet_length - 1, col=facet_length - 1),
                Coord(row=0, col=-1),
            ),
            "left": (Coord(row=facet_length - 1, col=0), Coord(row=-1, col=0)),
        }

        # Iterate over all facets
        for facet1 in cube_grid.values():
            coord = facet1.coord
            # Translate from the small cube to the large cube coordinate system
            offset1 = Coord(row=coord.row * facet_length, col=coord.col * facet_length)

            # Iterate over all sides of the facets
            for side1, (coord1, delta1) in side_orientation.items():
                coord1 += offset1

                # If the first node on the facet of the large cube
                # is already glued, we can skip the whole side
                if getattr(grid[coord1], side1) is not None:
                    continue

                # Using the small cube, identify the correct
                # other facet to glue to
                facet2 = getattr(facet1, side1)
                offset2 = Coord(
                    row=facet2.coord.row * facet_length,
                    col=facet2.coord.col * facet_length,
                )
                side2 = facet1.other_side[side1]
                coord2, delta2 = side_orientation[side2]
                # We have to traverse the other edge against orientation
                # Hence, other start point
                coord2 += offset2 + delta2 * (facet_length - 1)

                # Glue all nodes on the edge of the two facets
                for _ in range(facet_length):
                    setattr(grid[coord1], side1, grid[coord2])
                    setattr(grid[coord2], side2, grid[coord1])
                    grid[coord1].other_side[side1] = side2
                    grid[coord2].other_side[side2] = side1
                    coord1 += delta1
                    # Again other sign, because of opposite orientation
                    coord2 -= delta2


@click.command()
@click.argument("fn", type=click.Path(exists=True, dir_okay=False))
def main(fn: str):
    """Solution for day 22

    FN should point to the input file for today.

    \f
    Parameters
    ----------
    fn : str
        Path to input file
    """
    in_fn = Path(fn)

    maze_lines: list[str] = []
    with open(in_fn, "r") as f:
        for line in f.readlines():
            # line = line.strip()
            if "." not in line:
                instructions = line.strip()
                continue

            maze_lines.append(line)

    maze = Maze(maze_lines)
    password = maze.follow_path(instructions)
    print(f"Task01: {password}")

    cube = Cube(maze_lines=maze_lines)
    password = cube.follow_path(instructions)
    print(f"Task02: {password}")


if __name__ == "__main__":
    main()
