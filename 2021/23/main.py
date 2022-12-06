"""Solutions on day 23.

https://adventofcode.com/2021/day/23


+---------------+
|01  2  3  4  56|
+-+A0+B0+C0+D0+-+
  |A1|B1|C1|D1|
  |A2|B2|C2|D2|
  |A3|B3|C3|D3|
  +--+--+--+--+
"""
from copy import deepcopy
from dataclasses import dataclass
from functools import cache
from heapq import heappop, heappush
from pathlib import Path
from time import perf_counter_ns
from typing import Dict, Iterator, List, Optional, Tuple, Union

import click


class Burrow:
    # First Hall square and distance to end.
    HALL_DIST: dict[tuple[str, int], tuple[int, int]] = {
        ("A", 0): (1, 3),
        ("A", 1): (1, 2),
        ("A", 2): (2, 2),
        ("A", 3): (2, 4),
        ("A", 4): (2, 6),
        ("A", 5): (2, 8),
        ("A", 6): (2, 9),
        ("B", 0): (2, 5),
        ("B", 1): (2, 4),
        ("B", 2): (2, 2),
        ("B", 3): (3, 2),
        ("B", 4): (3, 4),
        ("B", 5): (3, 6),
        ("B", 6): (3, 7),
        ("C", 0): (3, 7),
        ("C", 1): (3, 6),
        ("C", 2): (3, 4),
        ("C", 3): (3, 2),
        ("C", 4): (4, 2),
        ("C", 5): (4, 4),
        ("C", 6): (4, 5),
        ("D", 0): (4, 9),
        ("D", 1): (4, 8),
        ("D", 2): (4, 6),
        ("D", 3): (4, 4),
        ("D", 4): (4, 2),
        ("D", 5): (5, 2),
        ("D", 6): (5, 3),
    }

    COST_FACTOR: dict[str, int] = {
        "A": 1,
        "B": 10,
        "C": 100,
        "D": 1000,
    }

    INBETWEEN_POS: dict[frozenset[str, str], int] = {
        frozenset(["A", "B"]): 2,
        frozenset(["A", "C"]): 2,
        frozenset(["A", "D"]): 2,
        frozenset(["B", "C"]): 3,
        frozenset(["B", "D"]): 3,
        frozenset(["C", "D"]): 4,
    }

    ADDITIONAL_LINES = [
        "  #D#C#B#A#",
        "  #D#B#A#C#",
    ]

    def __init__(
        self,
        state: dict[str, list[str | None]],
        cost: int = 0,
        est_remainder: Optional[int] = None,
    ) -> None:
        self.state = deepcopy(state)
        self.cost = cost
        if est_remainder is None:
            self.est_remainder = self.estimate_remainder_cost()
        else:
            self.est_remainder = est_remainder
        self.state_hash = self._state_hash()

    @classmethod
    def from_input(cls, burrow_txt: str, second_task: bool = False) -> "Burrow":
        state: dict[str, list[str | None]] = {
            "H": [None] * 7,
            "A": [],
            "B": [],
            "C": [],
            "D": [],
        }
        lines = burrow_txt.split("\n")

        offset = 3
        if second_task:
            lines = lines[:3] + cls.ADDITIONAL_LINES + lines[3:]
            offset = 5

        for i, line in enumerate(lines):
            if i < 2 or i > offset:
                continue
            components = line.strip().strip("#").split("#")
            for amphipod, room in zip(components, "ABCDC"):
                state[room].append(amphipod)

        return Burrow(state)

    @classmethod
    @cache
    def movement_cost(
        cls, room_type: str, room_depth: int, hall_field: int, aquapod: str
    ) -> int:
        _, dist = cls.HALL_DIST[(room_type, hall_field)]
        return (dist + room_depth) * cls.COST_FACTOR[aquapod]

    def estimate_remainder_cost(self) -> int:
        est = 0
        for hall_pos, amphipod in enumerate(self.state["H"]):
            if amphipod is None:
                continue
            est += self.movement_cost(
                room_type=amphipod,
                room_depth=0,
                hall_field=hall_pos,
                aquapod=amphipod,
            )

        for room_type in "ABCD":
            for room_depth, amphipod in enumerate(self.state[room_type]):
                if amphipod is None or amphipod == room_type:
                    continue

                hall_pos = self.INBETWEEN_POS[frozenset([amphipod, room_type])]
                est += self.movement_cost(
                    room_type=room_type,
                    room_depth=room_depth,
                    hall_field=hall_pos,
                    aquapod=amphipod,
                )
                est += self.movement_cost(
                    room_type=amphipod,
                    room_depth=0,
                    hall_field=hall_pos,
                    aquapod=amphipod,
                )

        return est

    def path_free(self, room_type: str, hall_field: int) -> bool:
        hall_start, _ = self.HALL_DIST[(room_type, hall_field)]
        if hall_start <= hall_field:
            res = all(
                square is None for square in self.state["H"][hall_start:hall_field]
            )
        else:
            res = all(
                square is None for square in self.state["H"][hall_start:hall_field:-1]
            )
        return res

    def __str__(self) -> str:
        h = [amphipod if amphipod is not None else " " for amphipod in self.state["H"]]

        lines = [
            "#" * 13,
            "#{}{} {} {} {} {}{}#".format(*h),
        ]
        for i in range(len(self.state["A"])):
            occupants = [
                self.state[room_type][i]
                if self.state[room_type][i] is not None
                else " "
                for room_type in "ABCD"
            ]
            if i == 0:
                lines.append("###{}#{}#{}#{}###".format(*occupants))
            else:
                lines.append("  #{}#{}#{}#{}#".format(*occupants))

        lines.append("  " + "#" * 9)

        return "\n".join(lines)

    def is_sorted(self) -> bool:
        for room_type in "ABCD":
            if any(amphipod != room_type for amphipod in self.state[room_type]):
                return False

        return True

    def room_open(self, room_type: str) -> bool:
        return all(amphipod in [None, room_type] for amphipod in self.state[room_type])

    def __lt__(self, other: "Burrow") -> bool:
        return self.cost + self.est_remainder < other.cost + other.est_remainder

    def _state_hash(self) -> int:
        # return hash(
        #     tuple(
        #         amphipod for room_type in "ABCDH" for amphipod in self.state[room_type]
        #     )
        # )
        # return id(self.state)
        # return hash(str(self))
        return hash(tuple(tuple(self.state[room_type]) for room_type in "ABCDH"))

    def __hash__(self) -> int:
        return self.state_hash

    def next_open_spot_in_room(self, room_type: str) -> int:
        return len(self.state[room_type]) - 1 - self.state[room_type][::-1].index(None)

    def move(self, from_room: str, from_pos: int, to_room: str, to_pos: int):
        amphipod = self.state[from_room][from_pos]
        assert amphipod is not None
        if from_room == "H":
            self.cost += Burrow.movement_cost(
                room_type=to_room,
                room_depth=to_pos,
                hall_field=from_pos,
                aquapod=amphipod,
            )
            self.est_remainder -= Burrow.movement_cost(
                room_type=to_room,
                room_depth=0,
                hall_field=from_pos,
                aquapod=amphipod,
            )
        else:
            self.cost += Burrow.movement_cost(
                room_type=from_room,
                room_depth=from_pos,
                hall_field=to_pos,
                aquapod=amphipod,
            )
            self.est_remainder += Burrow.movement_cost(
                room_type=amphipod,
                room_depth=0,
                hall_field=to_pos,
                aquapod=amphipod,
            )
            if from_room != amphipod:
                hall_pos = self.INBETWEEN_POS[frozenset([from_room, amphipod])]
                self.est_remainder -= Burrow.movement_cost(
                    room_type=from_room,
                    room_depth=from_pos,
                    hall_field=hall_pos,
                    aquapod=amphipod,
                )
                self.est_remainder -= Burrow.movement_cost(
                    room_type=amphipod,
                    room_depth=0,
                    hall_field=hall_pos,
                    aquapod=amphipod,
                )

        self.state[to_room][to_pos] = self.state[from_room][from_pos]
        self.state[from_room][from_pos] = None
        self.state_hash = self._state_hash()

    def valid_states(self) -> list["Burrow"]:
        next_states = []

        # Try to move hallway points into rooms
        for hall_pos in range(len(self.state["H"])):
            amphipod = self.state["H"][hall_pos]
            # Check if there is amphipod and the room for the amphipod does not contain false amphipods
            if amphipod is not None and self.room_open(amphipod):
                # Check if path to burrow is free
                if self.path_free(amphipod, hall_pos):
                    # Go as deep as possible
                    room_depth = self.next_open_spot_in_room(amphipod)
                    # Create new burrow and perform the move
                    new_burrow = Burrow(
                        state=self.state,
                        cost=self.cost,
                        est_remainder=self.est_remainder,
                    )
                    new_burrow.move(
                        from_room="H",
                        from_pos=hall_pos,
                        to_room=amphipod,
                        to_pos=room_depth,
                    )
                    next_states.append(new_burrow)

        # Try to move amphipod out of their rooms to create open rooms
        for room_type in "ABCD":
            # If the room is open, skip it
            if self.room_open(room_type):
                continue
            # print(room_type)
            # Find first amphipod in room
            for room_depth, amphipod in enumerate(self.state[room_type]):
                if amphipod is not None:
                    break

            # print(f"{room_depth=}")
            # print(f"{amphipod=}")

            for hall_pos in range(len(self.state["H"])):
                # print(
                #     f"{hall_pos=}\t{self.state['H'][hall_pos]=}\t{self.path_free(room_type, hall_pos)=}"
                # )
                if self.state["H"][hall_pos] is None and self.path_free(
                    room_type, hall_pos
                ):
                    # print("blubb")
                    new_burrow = Burrow(
                        state=self.state,
                        cost=self.cost,
                        est_remainder=self.est_remainder,
                    )
                    new_burrow.move(
                        from_room=room_type,
                        from_pos=room_depth,
                        to_room="H",
                        to_pos=hall_pos,
                    )

                    next_states.append(new_burrow)

        return next_states

    def sort_amphipods(self) -> "Burrow":
        visited = set()
        heap = [self]

        i = 0

        while heap:
            burrow = heappop(heap)
            visited.add(burrow)

            if i % 10000 == 0:
                print(burrow)
                print(f"{burrow.cost=}")
                print(f"{burrow.est_remainder=}\n")

            if burrow.is_sorted():
                return burrow

            for next_burrow in burrow.valid_states():
                if next_burrow not in visited:
                    heappush(heap, next_burrow)

            i += 1
        return Burrow(self.state, cost=-1)


# @dataclass
# class Square:
#     """Square of the burrow."""

#     kind: str
#     coord: Tuple[int, int]
#     neighbors: List["Square"]
#     state: Optional[str] = None

#     def __repr__(self) -> str:
#         """Print square."""
#         return f"Square(kind={self.kind}, coord={self.coord}, " f"state={self.state})"


# class Burrow:
#     """State of the burrow."""

#     def __init__(
#         self, start_positions: Dict[Tuple[int, int], str], total_energy: int = 0
#     ):
#         """Initialize burrow."""
#         temp_square = Square("temp", coord=(-1, -1), neighbors=[])
#         prev_square = temp_square
#         self.room_depth = max(y for _, y in start_positions.keys())
#         self.coord_dict: Dict[Tuple[int, int], Square] = {}
#         for i in range(11):
#             kind = "entrance" if i in [2, 4, 6, 8] else "hallway"
#             square = Square(kind=kind, coord=(i, 0), neighbors=[prev_square])
#             prev_square.neighbors.append(square)
#             self.coord_dict[(i, 0)] = square

#             if i in [2, 4, 6, 8]:
#                 vert_square = square
#                 idx = i // 2 - 1
#                 kind = chr(65 + idx)
#                 for j in range(1, self.room_depth + 1):
#                     coord = (i, j)
#                     room_square = Square(kind, coord, neighbors=[vert_square])
#                     vert_square.neighbors.append(room_square)
#                     self.coord_dict[coord] = room_square
#                     vert_square = room_square

#             prev_square = square

#         self.entry_point = temp_square.neighbors[0]
#         self.entry_point.neighbors.remove(temp_square)

#         for coord, amphipod in start_positions.items():
#             self.coord_dict[coord].state = amphipod

#         self.total_energy = 0
#         # Save the path as a string representation for debugging
#         self.previous = ""

#     def __repr__(self) -> str:
#         """Print burrow."""
#         grid = [
#             [" "] * 11,
#             ["-", "+", " ", "+", " ", "+", " ", "+", " ", "+", "-"],
#         ]
#         for _ in range(self.room_depth - 1):
#             grid.append(list([" ", "|", " ", "|", " ", "|", " ", "|", " ", "|", ""]))

#         for coord, square in self.coord_dict.items():
#             if square.state is None:
#                 continue

#             x, y = coord

#             grid[y][x] = square.state

#         res = "+" + "-" * 11 + "+\n"
#         res += "|" + "".join(grid[0]) + "|\n"
#         res += "+" + "".join(grid[1]) + "+\n"
#         for i in range(2, self.room_depth + 1):
#             res += " " + "".join(grid[i]) + "\n"
#         res += "  +-+-+-+-+\n"
#         return res

#     def __str__(self) -> str:
#         """Transform to string representation."""
#         return self.__repr__()

#     def _home_filled(self, square: Square) -> bool:
#         if square.kind != square.state:
#             return False

#         if square.coord[1] == self.room_depth:
#             return True

#         x = square.coord[0]
#         y = square.coord[1] + 1
#         square = self.coord_dict[(x, y)]
#         return self._home_filled(square)

#     def _swap(self, square: Square, new_square: Square) -> "Burrow":
#         burrow = deepcopy(self)
#         burrow.coord_dict[new_square.coord].state = square.state
#         burrow.coord_dict[square.coord].state = None
#         energy = (
#             abs(square.coord[0] - new_square.coord[0])
#             + square.coord[1]
#             + new_square.coord[1]
#         )
#         energy *= 10 ** (ord(square.state) - 65)
#         burrow.total_energy += energy
#         burrow.previous = self.previous + "\n\n" + str(self) + f"{energy=}"
#         return burrow

#     def valid_moves(
#         self, start_coord: Optional[Tuple[int, int]] = None
#     ) -> Iterator["Burrow"]:
#         """Compute all valid moves.

#         If coord is given, compute only moves for amphipods at given coordinate
#         (if there is one).
#         """
#         coord_dict = self.coord_dict
#         if start_coord is not None:
#             coord_dict = {start_coord: self.coord_dict[start_coord]}

#         for coord, square in coord_dict.items():
#             if square.state is None or self._home_filled(square):
#                 continue

#             x_home = 2 * (ord(square.state) - 64)
#             if square.kind == "hallway":
#                 sx = square.coord[0]
#                 if x_home > sx:
#                     left = sx + 1
#                     right = x_home
#                 else:
#                     left = x_home + 1
#                     right = sx
#                 if self.coord_dict[(x_home, 1)].state is not None or any(
#                     self.coord_dict[(t, 0)].state is not None
#                     for t in range(left, right)
#                 ):
#                     continue

#                 for y in range(1, self.room_depth + 1):
#                     new_square = self.coord_dict[(x_home, y)]
#                     if new_square.state is not None:
#                         break

#                     yield self._swap(square, new_square)

#                 continue

#             x, y = square.coord
#             left = max(
#                 [-1]
#                 + [t for t in range(x) if self.coord_dict[(t, 0)].state is not None]
#             )
#             if left <= 0:
#                 left += 1
#             else:
#                 left += 2

#             right = min(
#                 [11]
#                 + [
#                     t
#                     for t in range(x + 1, 11)
#                     if self.coord_dict[(t, 0)].state is not None
#                 ]
#             )
#             if any(self.coord_dict[x, t].state is not None for t in range(0, y)) or (
#                 left == y + 1 and right == y + 1
#             ):
#                 continue

#             xs = []
#             if left == 0:
#                 xs.append(0)
#                 left = 1

#             xs += list(range(left, right, 2))

#             if right == 11:
#                 xs.append(10)

#             for t in xs:
#                 new_square = self.coord_dict[(t, 0)]
#                 yield self._swap(square, new_square)

#             if x_home < left or right <= x_home:
#                 continue

#             for t in range(1, self.room_depth + 1):
#                 new_square = self.coord_dict[(x_home, t)]
#                 if new_square.state is not None:
#                     break
#                 yield self._swap(square, new_square)

#     def is_sorted(self) -> bool:
#         """Check if burrow is sorted."""
#         for square in self.coord_dict.values():
#             if square.state is not None and square.state != square.kind:
#                 return False

#         return True

#     @staticmethod
#     def sort_amphipods(start_positions: List[Tuple[str, str]]) -> "Burrow":
#         """Sort amphipods with minimal energy."""
#         burrow = Burrow(start_positions)
#         visited = set()

#         heap = [burrow]

#         while len(heap):
#             burrow = heappop(heap)

#             fingerprint = str(burrow)
#             if fingerprint in visited:
#                 continue

#             visited.add(fingerprint)

#             if burrow.is_sorted():
#                 return burrow

#             for new_burrow in burrow.valid_moves():
#                 heappush(heap, new_burrow)

#     def __lt__(self, other: "Burrow") -> bool:
#         """Neede for heap."""
#         return self.total_energy < other.total_energy


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
        txt_grid = f.read()
        burrow = Burrow.from_input(deepcopy(txt_grid))
        burrow2 = Burrow.from_input(deepcopy(txt_grid), second_task=True)
        # for i, line in enumerate(f.readlines()):
        #     if i < 2 or i > 3:
        #         continue
        #     components = line.strip().strip("#").split("#")
        #     for j, amphipod in enumerate(components):
        #         x = 2 * j + 2
        #         y = i - 1
        #         start_positions[(x, y)] = amphipod
        #         if y == 2:
        #             start_positions_long[(x, y + 2)] = amphipod
        #         else:
        #             start_positions_long[(x, y)] = amphipod

    if task01:
        print("\nTask 01")
        # burrow = Burrow(start_positions)
        print(burrow)
        print(f"{burrow.cost=}")
        print(f"{burrow.est_remainder=}")

        # next_burrows = burrow.valid_states()
        # for nb in next_burrows[:2]:
        #     print(nb)
        #     print(nb.cost)
        # print(nb.next_open_spot_in_room("A"))

        t_start = perf_counter_ns()
        sorted_burrow = burrow.sort_amphipods()
        t_stop = perf_counter_ns()
        print(sorted_burrow)
        print(f"{sorted_burrow.cost=}")
        delta = (t_stop - t_start) * 10 ** (-9)
        print(f"Running time: {delta}s")

        # burrow = Burrow.sort_amphipods(start_positions)
        # print(burrow.previous)
        # print(f"{burrow.total_energy=}")

        # test_burrow = Burrow(
        #     state={
        #         "H": [None, None, "B", "C", None, None, None],
        #         "A": ["B", "A"],
        #         "B": [None, "D"],
        #         "C": [None, "C"],
        #         "D": ["D", "A"],
        #     },
        #     cost=240,
        # )
        # print(test_burrow)
        # print(f"{test_burrow.cost=}")

        # for nb in test_burrow.valid_states():
        #     print(nb)
        #     print(f"{nb.cost=}")

    if task02:
        print("\nTask 02")
        print(burrow2)

        t_start = perf_counter_ns()
        sorted_burrow = burrow2.sort_amphipods()
        t_stop = perf_counter_ns()
        print(sorted_burrow)
        print(f"{sorted_burrow.cost=}")
        delta = (t_stop - t_start) * 10 ** (-9)
        print(f"Running time: {delta}s")
        burrow_long = Burrow(start_positions_long)
        print(burrow_long)

        # burrow_long = Burrow.sort_amphipods(start_positions_long)
        # print(burrow_long.previous)
        # print(f"{burrow_long.total_energy=}")

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
