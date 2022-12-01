"""Solutions on day 23.

https://adventofcode.com/2021/day/23
"""
from dataclasses import dataclass
from heapq import heappop, heappush
from pathlib import Path
from typing import Iterator, List, Optional, Tuple, Union, Dict

import click
from copy import deepcopy


@dataclass
class Square:
    """Square of the burrow."""

    kind: str
    coord: Tuple[int, int]
    neighbors: List["Square"]
    state: Optional[str] = None

    def __repr__(self) -> str:
        """Print square."""
        return f"Square(kind={self.kind}, coord={self.coord}, " f"state={self.state})"


class Burrow:
    """State of the burrow."""

    def __init__(
        self, start_positions: Dict[Tuple[int, int], str], total_energy: int = 0
    ):
        """Initialize burrow."""
        temp_square = Square("temp", coord=(-1, -1), neighbors=[])
        prev_square = temp_square
        self.room_depth = max(y for _, y in start_positions.keys())
        self.coord_dict: Dict[Tuple[int, int], Square] = {}
        for i in range(11):
            kind = "entrance" if i in [2, 4, 6, 8] else "hallway"
            square = Square(kind=kind, coord=(i, 0), neighbors=[prev_square])
            prev_square.neighbors.append(square)
            self.coord_dict[(i, 0)] = square

            if i in [2, 4, 6, 8]:
                vert_square = square
                idx = i // 2 - 1
                kind = chr(65 + idx)
                for j in range(1, self.room_depth + 1):
                    coord = (i, j)
                    room_square = Square(kind, coord, neighbors=[vert_square])
                    vert_square.neighbors.append(room_square)
                    self.coord_dict[coord] = room_square
                    vert_square = room_square

            prev_square = square

        self.entry_point = temp_square.neighbors[0]
        self.entry_point.neighbors.remove(temp_square)

        for coord, amphipod in start_positions.items():
            self.coord_dict[coord].state = amphipod

        self.total_energy = 0
        # Save the path as a string representation for debugging
        self.previous = ""

    def __repr__(self) -> str:
        """Print burrow."""
        grid = [
            [" "] * 11,
            ["-", "+", " ", "+", " ", "+", " ", "+", " ", "+", "-"],
        ]
        for _ in range(self.room_depth - 1):
            grid.append(list([" ", "|", " ", "|", " ", "|", " ", "|", " ", "|", ""]))

        for coord, square in self.coord_dict.items():
            if square.state is None:
                continue

            x, y = coord

            grid[y][x] = square.state

        res = "+" + "-" * 11 + "+\n"
        res += "|" + "".join(grid[0]) + "|\n"
        res += "+" + "".join(grid[1]) + "+\n"
        for i in range(2, self.room_depth + 1):
            res += " " + "".join(grid[i]) + "\n"
        res += "  +-+-+-+-+\n"
        return res

    def __str__(self) -> str:
        """Transform to string representation."""
        return self.__repr__()

    def _home_filled(self, square: Square) -> bool:
        if square.kind != square.state:
            return False

        if square.coord[1] == self.room_depth:
            return True

        x = square.coord[0]
        y = square.coord[1] + 1
        square = self.coord_dict[(x, y)]
        return self._home_filled(square)

    def _swap(self, square: Square, new_square: Square) -> "Burrow":
        burrow = deepcopy(self)
        burrow.coord_dict[new_square.coord].state = square.state
        burrow.coord_dict[square.coord].state = None
        energy = (
            abs(square.coord[0] - new_square.coord[0])
            + square.coord[1]
            + new_square.coord[1]
        )
        energy *= 10 ** (ord(square.state) - 65)
        burrow.total_energy += energy
        burrow.previous = self.previous + "\n\n" + str(self) + f"{energy=}"
        return burrow

    def valid_moves(
        self, start_coord: Optional[Tuple[int, int]] = None
    ) -> Iterator["Burrow"]:
        """Compute all valid moves.

        If coord is given, compute only moves for amphipods at given coordinate
        (if there is one).
        """
        coord_dict = self.coord_dict
        if start_coord is not None:
            coord_dict = {start_coord: self.coord_dict[start_coord]}

        for coord, square in coord_dict.items():
            if square.state is None or self._home_filled(square):
                continue

            x_home = 2 * (ord(square.state) - 64)
            if square.kind == "hallway":
                sx = square.coord[0]
                if x_home > sx:
                    left = sx + 1
                    right = x_home
                else:
                    left = x_home + 1
                    right = sx
                if self.coord_dict[(x_home, 1)].state is not None or any(
                    self.coord_dict[(t, 0)].state is not None
                    for t in range(left, right)
                ):
                    continue

                for y in range(1, self.room_depth + 1):
                    new_square = self.coord_dict[(x_home, y)]
                    if new_square.state is not None:
                        break

                    yield self._swap(square, new_square)

                continue

            x, y = square.coord
            left = max(
                [-1]
                + [t for t in range(x) if self.coord_dict[(t, 0)].state is not None]
            )
            if left <= 0:
                left += 1
            else:
                left += 2

            right = min(
                [11]
                + [
                    t
                    for t in range(x + 1, 11)
                    if self.coord_dict[(t, 0)].state is not None
                ]
            )
            if any(self.coord_dict[x, t].state is not None for t in range(0, y)) or (
                left == y + 1 and right == y + 1
            ):
                continue

            xs = []
            if left == 0:
                xs.append(0)
                left = 1

            xs += list(range(left, right, 2))

            if right == 11:
                xs.append(10)

            for t in xs:
                new_square = self.coord_dict[(t, 0)]
                yield self._swap(square, new_square)

            if x_home < left or right <= x_home:
                continue

            for t in range(1, self.room_depth + 1):
                new_square = self.coord_dict[(x_home, t)]
                if new_square.state is not None:
                    break
                yield self._swap(square, new_square)

    def is_sorted(self) -> bool:
        """Check if burrow is sorted."""
        for square in self.coord_dict.values():
            if square.state is not None and square.state != square.kind:
                return False

        return True

    @staticmethod
    def sort_amphipods(start_positions: List[Tuple[str, str]]) -> "Burrow":
        """Sort amphipods with minimal energy."""
        burrow = Burrow(start_positions)
        visited = set()

        heap = [burrow]

        while len(heap):
            burrow = heappop(heap)

            fingerprint = str(burrow)
            if fingerprint in visited:
                continue

            visited.add(fingerprint)

            if burrow.is_sorted():
                return burrow

            for new_burrow in burrow.valid_moves():
                heappush(heap, new_burrow)

    def __lt__(self, other: "Burrow") -> bool:
        """Neede for heap."""
        return self.total_energy < other.total_energy


@click.command()
@click.argument("path", type=click.Path())
@click.option("--task01/--no-task01", default=True, help="Run task 01.")
@click.option("--task02/--no-task02", default=False, help="Run task 02.")
def main(path: Union[str, Path], task01: bool, task02: bool):
    """Solve day 23 tasks.

    Read input from PATH and prints the solution.
    \f

    :param path: Path to the input file
    :param task01: Run task 01
    :param task02: Run task 02
    """
    path = Path(path)

    start_positions = {}
    start_positions_long = {
        (2, 2): "D",
        (2, 3): "D",
        (4, 2): "C",
        (4, 3): "B",
        (6, 2): "B",
        (6, 3): "A",
        (8, 2): "A",
        (8, 3): "C",
    }
    with open(path, "r") as f:
        for i, line in enumerate(f.readlines()):
            if i < 2 or i > 3:
                continue
            components = line.strip().strip("#").split("#")
            for j, amphipod in enumerate(components):
                x = 2 * j + 2
                y = i - 1
                start_positions[(x, y)] = amphipod
                if y == 2:
                    start_positions_long[(x, y + 2)] = amphipod
                else:
                    start_positions_long[(x, y)] = amphipod

    if task01:
        print("\nTask 01")
        burrow = Burrow(start_positions)
        print(burrow)

        burrow = Burrow.sort_amphipods(start_positions)
        print(burrow.previous)
        print(f"{burrow.total_energy=}")

    if task02:
        print("\nTask 02")
        burrow_long = Burrow(start_positions_long)
        print(burrow_long)

        burrow_long = Burrow.sort_amphipods(start_positions_long)
        print(burrow_long.previous)
        print(f"{burrow_long.total_energy=}")

    # start_positions = {
    #     (8, 2): 'A',
    #     (2, 2): 'A',
    #     (4, 1): 'B',
    #     (4, 2): 'B',
    #     (6, 1): 'C',
    #     (6, 2): 'C',
    #     (8, 1): 'D',
    #     (5, 0): 'D'
    # }

    # burrow = Burrow(start_positions)
    # print(burrow)

    # print('\nTask 01')
    # burrow = Burrow.sort_amphipods(start_positions)
    # print(f'{burrow.total_energy=}')
    # print(burrow.previous)

    # for new_burrow in burrow.valid_moves():
    #     print(new_burrow)
    #     print(f'{new_burrow.total_energy=}')
    #     print(new_burrow.is_sorted())

    #     # print(new_burrow.previous)


if __name__ == "__main__":
    main()
