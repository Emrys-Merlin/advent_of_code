import re
from dataclasses import dataclass
from heapq import heappop, heappushpop
from math import lcm
from pathlib import Path
from typing import Literal

from loguru import logger
from typer import Typer

main = Typer()


@dataclass
class Node:
    """Wrapper to navigate graph"""

    left: str
    right: str

    def __getitem__(self, key: Literal["L", "R"]) -> str:
        if key == "L":
            return self.left

        return self.right


@dataclass
class State:
    """State for the iterative ghost path approach.

    Still way too slow, so will most likely be removed later.
    """

    node: str
    distance: int

    def __lt__(self, other: "State") -> bool:
        return self.distance < other.distance


@dataclass
class Map:
    """The map we or the ghosts are following

    instructions: String of "L"s and "R"s describing turns
    graph: The graph on which we move

    """

    instruction: str
    graph: dict[str, Node]

    def __len__(self) -> int:
        return len(self.instruction)

    def __getitem__(self, key: int) -> str:
        """Wrap the instructions cyclically"""
        return self.instruction[key % len(self)]

    def follow_path(
        self,
        start: str = "AAA",
        end: frozenset[str] = frozenset(("ZZZ",)),
        ignore_first: bool = False,
    ) -> tuple[int, str]:
        """Follow path as described in task 01

        Args:
            start: Start node. Defaults to "AAA".
            end: Set of end nodes. Defaults to frozenset(("ZZZ",)).
            ignore_first: If true, ignores if the first node is an end note. Defaults to False.

        Returns:
            (n_steps_to_end, end_node)
        """
        if not ignore_first and start in end:
            return 0, start

        current = start

        for i, instr in enumerate(self, 1):
            node = self.graph[current]

            if instr == "L":
                current = node.left
            else:
                current = node.right

            if current in end:
                return i, current

    def follow_ghost_path_brute_force(self) -> int:
        """Brute force approach

        We simply follow each path from start and check if
        all of them end at the same time.

        Way too slow for the actual task.

        Returns:
            Path length
        """
        current = set(node for node in self.graph.keys() if node[-1] == "A")
        end = set(node for node in self.graph.keys() if node[-1] == "Z")

        if current.issubset(end):
            return 0

        for i, instr in enumerate(self, 1):
            current = set(self.graph[node][instr] for node in current)

            if current.issubset(end):
                return i

    def follow_ghost_path_iterative(self) -> int:
        """We walk the shortest path

        Using a heap we extend the shortest path to the next
        end node and check if all paths have reached the same
        length.

        Also way too slow

        Returns:
            Path length
        """
        ends = frozenset(node for node in self.graph.keys() if node[-1] == "Z")

        heap = [
            State(node=node, distance=0)
            for node in self.graph.keys()
            if node[-1] == "A"
        ]

        max_dist = -1
        current = heappop(heap)
        while True:
            if current.distance == max_dist:
                break

            distance, new_node = self.follow_path(current.node, ends, ignore_first=True)

            new_dist = current.distance + distance
            max_dist = max(max_dist, new_dist)

            current = heappushpop(heap, State(new_node, new_dist))

        return max_dist

    def cycle_offset(self, start: str, ends: frozenset[str]) -> tuple[int, int]:
        """Compute the cycle length and the offset of a path

        As the graph is finite, each path has to lead to a cycle
        where the same node is reached multiple times. The path
        until the cycle starts is called offset.

        The method computes a cycle by checking when a path reaches
        the same end node a second time and saving the steps until
        the first and second occurence

        Args:
            start: Start node
            ends: Set of end nodes

        Returns:
            (cycle_length, offset)
        """
        offset = 0
        ends_reached: dict[str, int] = {}
        while True:
            n, end = self.follow_path(start, ends, ignore_first=True)
            offset += n
            if end in ends_reached:
                break

            ends_reached[end] = offset
            start = end

        return offset - ends_reached[end], ends_reached[end]

    def follow_ghost_path(self) -> int:
        """Compute ghost path length based on cycle lengths + offset

        If there are no offsets (or the offsets are constant) or the
        offsets are as long as the respective cycle lengths, we can simply
        compute the path length as the least common multiple of the cycle
        lengths. This is implemented below, because it turned out that the
        cycle lenghts and the offsets are the same in the task data.

        For the more general case we are looking for x, s.t.
        x = m_i * cycle_length_i + offset_i
        where i enumerates the start points and m_i is an integer.
        This is equivalent to
        x = offset_i mod cycle_length_i
        This is a so called simultaneous congruence which can be solved
        using tools from the Chinese remainder theorem (if the solution
        exists). The solution only exists if
        offset_i = offset_j mod gcd(cycle_length_i, cycle_length_j)
        A solution can be constructed iteratively. This more general approach
        has not been implemented yet.

        Returns:
            path length
        """
        end = frozenset(node for node in self.graph.keys() if node[-1] == "Z")

        # Compute cycles and offsets
        cycles: dict[str, tuple[int, int]] = {}
        for node in self.graph.keys():
            if node[-1] != "A":
                continue

            cycles[node] = self.cycle_offset(node, end)

        # Check that the simplifying assumptions hold
        if (
            max(offset for _, offset in cycles.values())
            != min(offset for _, offset in cycles.values())
        ) and any(cycle != offset for cycle, offset in cycles.values()):
            raise NotImplementedError(
                "General chinese remainder approach not implemented yet."
            )

        # Compute path length via least common multiple
        return lcm(*[cycle for cycle, _ in cycles.values()])


def parse_input(input: str) -> Map:
    """Parse input into map

    Args:
        input: Input string

    Returns:
        Map object
    """
    instr, _, *lines = input.splitlines()

    nodes: dict[str, Node] = {}
    for line in lines:
        match = re.match(r"(\w+) = \((\w+), (\w+)\)", line)
        nodes[match.group(1)] = Node(match.group(2), match.group(3))

    return Map(instr, nodes)


@main.command()
def entrypoint(path: Path):
    """Entry point for python solutions"""
    with open(path, "r") as f:
        input = f.read().strip()

    map = parse_input(input)

    logger.info(f"Task 01: {map.follow_path()[0]}")
    logger.info(f"Task 02: {map.follow_ghost_path()}")


if __name__ == "__main__":
    main()
