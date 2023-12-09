import re
from dataclasses import dataclass
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


def extended_euclidean_algo(a: int, b: int) -> tuple[int, int, int]:
    """Extended Euclidean algorithm

    Args:
        a: First number
        b: Second number

    Returns:
        (gcd(a, b), x, y) s.t. a * x + b * y = gcd(a, b)
    """
    if a == 0:
        return b, 0, 1

    gcd, x1, y1 = extended_euclidean_algo(b % a, a)

    x = y1 - (b // a) * x1
    y = x1

    return gcd, x, y


def lowest_positive_remainder(remainder: int, modulus: int) -> int:
    """Compute lowest non-negative remainder

    Args:
        remainder: Remainder
        modulus: Modulus

    Returns:
        Lowest non-negative remainder
    """
    while remainder < 0:
        remainder += modulus

    while remainder - modulus > 0:
        remainder -= modulus

    return remainder


def chinese_remainder_theorem(
    remainders: list[int], moduli: list[int]
) -> tuple[int, int]:
    """Chinese remainder theorem

    Args:
        a: List of remainders
        n: List of moduli
        length: Length of lists

    Returns:
        (x, m) s.t. x = a_i mod n_i for all i and m = lcm(n_i)
    """
    if len(remainders) == len(moduli) == 1:
        return lowest_positive_remainder(remainders[0], moduli[0]), moduli[0]

    a, n = remainders.pop(), moduli.pop()
    b, m = remainders.pop(), moduli.pop()

    gcd, x, _ = extended_euclidean_algo(n, m)
    lcm = n * m // gcd

    if (a - b) % gcd != 0:
        raise ValueError("No solution exists")

    a_new = a - x * (a - b) // gcd * n
    a_new = lowest_positive_remainder(a_new, lcm)

    remainders.append(a_new)
    moduli.append(lcm)
    return chinese_remainder_theorem(remainders, moduli)


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

    def cycle_offset(self, start: str, ends: frozenset[str]) -> tuple[int, int]:
        """Compute the cycle length and the offset of a path

        As the graph is finite, each path has to lead to a cycle
        where the same node is reached multiple times. The path
        until the cycle starts is called offset.

        The method computes a cycle by checking when a path reaches
        the same end node a second time and saving the steps until
        the first and second occurence

        Caution! This method assumes that a cycle containing
        end points exists! Otherwise, infinite loop!

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
        A solution can be constructed iteratively.

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
        # to compute length over least common multiple
        if max(offset for _, offset in cycles.values()) == min(
            offset for _, offset in cycles.values()
        ):
            _, offset = next(iter(cycles.values()))
            return offset

        if all(cycle == offset for cycle, offset in cycles.values()):
            return lcm(*[cycle for cycle, _ in cycles.values()])

        moduli, remainders = list(zip(*cycles.values()))
        return chinese_remainder_theorem(
            remainders=list(remainders), moduli=list(moduli)
        )[0]


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
