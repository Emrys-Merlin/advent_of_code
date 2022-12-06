"""
Advent of Code 2021 - Day 23
https://adventofcode.com/2021/day/23

Solution was published at https://pastebin.com/hK1Y7St7
"""

import re
from copy import deepcopy
from queue import PriorityQueue
from typing import Any, Dict, List

DAY = "23"

FULL_INPUT_FILE = "./input.txt"
TEST_INPUT_FILE = "./example.txt"

PART_2_INSERTIONS = {
    "A": ["D", "D"],
    "B": ["C", "B"],
    "C": ["B", "A"],
    "D": ["A", "C"],
}


class Burrow:
    AMPHIPOD_TYPES = {"A": 1, "B": 10, "C": 100, "D": 1000}
    PATHS = {
        0: {"A": [0, 1], "B": [0, 1, 2], "C": [0, 1, 2, 3], "D": [0, 1, 2, 3, 4]},
        1: {"A": [1], "B": [1, 2], "C": [1, 2, 3], "D": [1, 2, 3, 4]},
        2: {"A": [2], "B": [2], "C": [2, 3], "D": [2, 3, 4]},
        3: {"A": [3, 2], "B": [3], "C": [3], "D": [3, 4]},
        4: {"A": [4, 3, 2], "B": [4, 3], "C": [4], "D": [4]},
        5: {"A": [5, 4, 3, 2], "B": [5, 4, 3], "C": [5, 4], "D": [5]},
        6: {"A": [6, 5, 4, 3, 2], "B": [6, 5, 4, 3], "C": [6, 5, 4], "D": [6, 5]},
    }

    def __init__(self, state: Dict = None, cost: int = 0):
        self.state = deepcopy(state) if state else {}
        self.cost = cost
        self.state_hash = self._state_hash

    def __lt__(self, other):
        return self.cost < other.cost

    @classmethod
    def move_cost(
        cls,
        hallway_position: int,
        room_type: str,
        room_position: int,
        amphipod_type: str,
    ):
        distance = (
            2 * len(cls.PATHS[hallway_position][room_type])
            + room_position
            - (1 if hallway_position in (0, 6) else 0)
        )
        return distance * cls.AMPHIPOD_TYPES[amphipod_type]

    @property
    def _state_hash(self):
        return hash(tuple(tuple(v) for k, v in sorted(self.state.items())))

    @property
    def is_a_winner(self):
        return all(all(b == a for b in self.state[a]) for a in self.AMPHIPOD_TYPES)

    @property
    def possible_moves(self) -> List[Any]:
        next_possible_states = []
        for hall_pos in range(len(self.state["H"])):
            amphipod_type = self.state["H"][hall_pos]
            if amphipod_type and self.room_open(amphipod_type):
                if self.path_to_room_clear(hall_pos, amphipod_type):
                    room_pos = self.next_spot_in_room(amphipod_type)
                    new_burrow = Burrow(self.state, self.cost)
                    new_burrow.move("H", hall_pos, amphipod_type, room_pos)
                    return [new_burrow]
        for room in self.AMPHIPOD_TYPES:
            if not self.room_open(room):
                amphipod_type = [_ for _ in self.state[room] if _][0]
                room_pos = self.state[room].index(amphipod_type)
                for hall_pos in self.PATHS:
                    if not self.state["H"][hall_pos] and self.path_to_room_clear(
                        hall_pos, room
                    ):
                        new_burrow = Burrow(self.state, self.cost)
                        new_burrow.move(room, room_pos, "H", hall_pos)
                        next_possible_states.append(new_burrow)
        return next_possible_states

    def move(self, from_room: str, from_pos: int, to_room: str, to_pos: int):
        self.cost += (
            self.move_cost(from_pos, to_room, to_pos, to_room)
            if from_room == "H"
            else self.move_cost(
                to_pos, from_room, from_pos, self.state[from_room][from_pos]
            )
        )
        self.state[to_room][to_pos] = self.state[from_room][from_pos]
        self.state[from_room][from_pos] = None
        self.state_hash = self._state_hash

    def room_open(self, room: str) -> bool:
        return all(_ in (None, room) for _ in self.state[room])

    def next_spot_in_room(self, room: str) -> int:
        return len(self.state[room]) - 1 - self.state[room][::-1].index(None)

    def path_to_room_clear(self, hallway_start: int, end_room_type: str) -> bool:
        for position in self.PATHS[hallway_start][end_room_type]:
            if position != hallway_start and self.state["H"][position]:
                return False
        return True


def find_path(burrow: Burrow) -> Burrow:
    queue = PriorityQueue()
    visited = set()
    queue.put(burrow)

    while queue:
        burrow = queue.get()
        if burrow.is_a_winner:
            return burrow
        elif burrow.state_hash not in visited:
            for possible_move in burrow.possible_moves:
                queue.put(possible_move)
            visited.add(burrow.state_hash)


def load_data(infile_path: str) -> Dict:
    with open(infile_path, "r", encoding="ascii") as infile:
        c = [
            re.match(r"\W*#+(\w)#(\w)#(\w)#(\w)#+", l).groups()
            for l in infile.readlines()[2:4]
        ]
        return {
            "H": [None] * 7,
            "A": [c[0][0], c[1][0]],
            "B": [c[0][1], c[1][1]],
            "C": [c[0][2], c[1][2]],
            "D": [c[0][3], c[1][3]],
        }


def part_1(infile_path: str) -> int:
    start_map = load_data(infile_path)
    result = find_path(Burrow(start_map))
    return result.cost


def part_2(infile_path: str) -> int:
    start_map = load_data(infile_path)
    for c in PART_2_INSERTIONS:
        start_map[c] = [start_map[c][0]] + PART_2_INSERTIONS[c] + [start_map[c][1]]
    result = find_path(Burrow(start_map))
    return result.cost


def show_moves(b):
    for i in range(len(b)):
        print(f"{i} : {b[i].cost} : {b[i].state}")


if __name__ == "__main__":
    part1_answer = part_1(FULL_INPUT_FILE)
    print(f"Part 1: {part1_answer}")

    part2_answer = part_2(FULL_INPUT_FILE)
    print(f"Part 2: {part2_answer}")
