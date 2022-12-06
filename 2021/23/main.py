"""Solutions on day 23.

https://adventofcode.com/2021/day/23

Solution inspired by and adapted from https://pastebin.com/hK1Y7St7

Representation of grid:
+---------------+
|01  2  3  4  56|
+-+A0+B0+C0+D0+-+
  |A1|B1|C1|D1|
  |A2|B2|C2|D2|
  |A3|B3|C3|D3|
  +--+--+--+--+
"""
from copy import deepcopy
from functools import cache
from heapq import heappop, heappush
from pathlib import Path
from time import perf_counter_ns
from typing import Optional, Union

import click


class Burrow:
    # Save distances from room entrances to hallway positions
    # Key: (room type, hall pos)
    # Value: (First hall square in path, distance)
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

    # Cost factors for the different amphipods
    COST_FACTOR: dict[str, int] = {
        "A": 1,
        "B": 10,
        "C": 100,
        "D": 1000,
    }

    # Arbitrary hall position between (room1, room2)
    # We need thes for the remainder cost estimate of the A*
    # algorithm
    INBETWEEN_POS: dict[frozenset[str, str], int] = {
        frozenset(["A", "B"]): 2,
        frozenset(["A", "C"]): 2,
        frozenset(["A", "D"]): 2,
        frozenset(["B", "C"]): 3,
        frozenset(["B", "D"]): 3,
        frozenset(["C", "D"]): 4,
    }

    # Additional lines for task 02
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
        """Initialized burrow

        Parameters
        ----------
        state : dict[str, list[str  |  None]]
            State of the burrow encoded as a dictionary with room_type as key and an array with the room positions. See top of file for grid representation.
        cost : int, optional
            Cost to end up at current configuration, by default 0
        est_remainder : Optional[int], optional
            Lower bound for remaining cost, if None, will be estimated internally, by default None
        """
        self.state = deepcopy(state)
        self.cost = cost
        if est_remainder is None:
            self.est_remainder = self.estimate_remainder_cost()
        else:
            self.est_remainder = est_remainder
        self.state_hash = self._state_hash()

    @classmethod
    def from_input(cls, burrow_txt: str, second_task: bool = False) -> "Burrow":
        """Transform text representation to Burrow

        Parameters
        ----------
        burrow_txt : str
            Text representation
        second_task : bool, optional
            If true, adds the additional lines, by default False

        Returns
        -------
        Burrow
            Burrow representation with 0 cost and remainder cost estimated.
        """
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
        cls, room_type: str, room_depth: int, hall_field: int, amphipod: str
    ) -> int:
        """Compute movement cost between an 'ABCD' room and the hallway

        Parameters
        ----------
        room_type : str
            Which room 'ABCD'
        room_depth : int
            How deep in the room is the amphipod
        hall_field : int
            Which hall field
        amphipod : str
            Which type of amphipod 'ABCD' moves

        Returns
        -------
        int
            Cost for move
        """
        _, dist = cls.HALL_DIST[(room_type, hall_field)]
        return (dist + room_depth) * cls.COST_FACTOR[amphipod]

    def estimate_remainder_cost(self) -> int:
        """Heuristic to estimate the remaining movement cost

        This heuristic computes a lower bound of the movement
        cost. Each amphipod takes the shortest path from its current
        position to the first spot in its room ignoring any other
        amphipods.

        Returns
        -------
        int
            Estimated remaining movement cost
        """
        est = 0
        for hall_pos, amphipod in enumerate(self.state["H"]):
            if amphipod is None:
                continue
            est += self.movement_cost(
                room_type=amphipod,
                room_depth=0,
                hall_field=hall_pos,
                amphipod=amphipod,
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
                    amphipod=amphipod,
                )
                est += self.movement_cost(
                    room_type=amphipod,
                    room_depth=0,
                    hall_field=hall_pos,
                    amphipod=amphipod,
                )

        return est

    def path_free(self, room_type: str, hall_field: int) -> bool:
        """Checks if the path in the hallway to/from room 'ABCD' is free

        Parameters
        ----------
        room_type : str
            Rom 'ABCD'
        hall_field : int
            Position in hall

        Returns
        -------
        bool
            True, if path is free
        """
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
        """Create string representation of grid

        Returns
        -------
        str
            String representation of grid
        """
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
        """Check if the grid is in the end state

        Returns
        -------
        bool
            True if all amphipods are in their correct room
        """
        for room_type in "ABCD":
            if any(amphipod != room_type for amphipod in self.state[room_type]):
                return False

        return True

    def room_open(self, room_type: str) -> bool:
        """Check if a room is does only contain amphipods of the correct type

        Parameters
        ----------
        room_type : str
            Room 'ABCD'

        Returns
        -------
        bool
            True, if the room contains only correct inhabitants
        """
        return all(amphipod in [None, room_type] for amphipod in self.state[room_type])

    def __lt__(self, other: "Burrow") -> bool:
        """Implement lower than based on previous movement cost + estimated remaining movement cost

        Parameters
        ----------
        other : Burrow
            Other burrow

        Returns
        -------
        bool
            True, if self < other
        """
        return self.cost + self.est_remainder < other.cost + other.est_remainder

    def _state_hash(self) -> int:
        """Encode current burrow state in a hash

        Returns
        -------
        int
            Hash of state
        """
        return hash(tuple(tuple(self.state[room_type]) for room_type in "ABCDH"))

    def __hash__(self) -> int:
        """Return cached hash

        Returns
        -------
        int
            Hash of sate
        """
        return self.state_hash

    def next_open_spot_in_room(self, room_type: str) -> int:
        """Get first empty room spot

        Parameters
        ----------
        room_type : str
            Room 'ABCD'

        Returns
        -------
        int
            First empty room spot
        """
        return len(self.state[room_type]) - 1 - self.state[room_type][::-1].index(None)

    def move(self, from_room: str, from_pos: int, to_room: str, to_pos: int):
        """Move amphipod to new position and compute costs and update remainder

        Caution! This method can only move an amphipod to or from the hallway.
        A move from room to room directly is not implemented.

        Parameters
        ----------
        from_room : str
            Room 'ABCD' or hallway 'H'
        from_pos : int
            Position in room
        to_room : str
            Room 'ABCD' or hallway 'H
        to_pos : int
            Position in room
        """
        amphipod = self.state[from_room][from_pos]
        assert amphipod is not None
        if from_room == "H":
            self.cost += Burrow.movement_cost(
                room_type=to_room,
                room_depth=to_pos,
                hall_field=from_pos,
                amphipod=amphipod,
            )
            self.est_remainder -= Burrow.movement_cost(
                room_type=to_room,
                room_depth=0,
                hall_field=from_pos,
                amphipod=amphipod,
            )
        else:
            self.cost += Burrow.movement_cost(
                room_type=from_room,
                room_depth=from_pos,
                hall_field=to_pos,
                amphipod=amphipod,
            )
            self.est_remainder += Burrow.movement_cost(
                room_type=amphipod,
                room_depth=0,
                hall_field=to_pos,
                amphipod=amphipod,
            )
            if from_room != amphipod:
                hall_pos = self.INBETWEEN_POS[frozenset([from_room, amphipod])]
                self.est_remainder -= Burrow.movement_cost(
                    room_type=from_room,
                    room_depth=from_pos,
                    hall_field=hall_pos,
                    amphipod=amphipod,
                )
                self.est_remainder -= Burrow.movement_cost(
                    room_type=amphipod,
                    room_depth=0,
                    hall_field=hall_pos,
                    amphipod=amphipod,
                )

        self.state[to_room][to_pos] = self.state[from_room][from_pos]
        self.state[from_room][from_pos] = None
        self.state_hash = self._state_hash()

    def valid_states(self) -> list["Burrow"]:
        """Get a list of valid new burrow states using a single amphipod move

        Returns
        -------
        list[Burrow]
            List of reachable burrow states
        """
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

    def sort_amphipods(self, max_iter: Optional[int] = None) -> "Burrow":
        """Find cheapest way to move amphipods to their proper room

        This method implements the A* algorithm.

        Parameters
        ----------
        max_iter : Optional[int], optional
            Max iterations for debug purposes, by default None

        Returns
        -------
        Burrow
            Final burrow, with movement cost
        """
        visited = set()
        heap = [self]

        i = 0

        while heap:
            burrow = heappop(heap)
            visited.add(hash(burrow))

            if i % 10000 == 0:
                print(burrow)
                print(f"{burrow.cost=}")
                print(f"{burrow.est_remainder=}\n")

            if burrow.is_sorted():
                return burrow

            for next_burrow in burrow.valid_states():
                if hash(next_burrow) not in visited:
                    heappush(heap, next_burrow)

            if max_iter is not None and i >= max_iter:
                break

            i += 1
        return Burrow(self.state, cost=-1)


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

    with open(path, "r") as f:
        txt_grid = f.read()
        burrow = Burrow.from_input(deepcopy(txt_grid))
        burrow2 = Burrow.from_input(deepcopy(txt_grid), second_task=True)

    if task01:
        print("\nTask 01")
        print(burrow)
        print(f"{burrow.cost=}")
        print(f"{burrow.est_remainder=}")

        t_start = perf_counter_ns()
        sorted_burrow = burrow.sort_amphipods(max_iter=None)
        t_stop = perf_counter_ns()
        print(sorted_burrow)
        print(f"{sorted_burrow.cost=}")
        delta = (t_stop - t_start) * 10 ** (-9)
        print(f"Running time: {delta}s")

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


if __name__ == "__main__":
    main()
