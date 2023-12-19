import re
from dataclasses import dataclass
from pathlib import Path
from typing import Iterator, Literal, Union

from loguru import logger
from typer import Typer
from utils import timer

main = Typer()


@dataclass
class Part:
    """Abstraction for a single part"""

    x: int
    m: int
    a: int
    s: int

    def __getitem__(self, i: Literal["x", "m", "a", "s"]):
        return getattr(self, i)

    @property
    def total(self) -> int:
        """Total value of a part"""
        return self.x + self.m + self.a + self.s

    @classmethod
    def from_line(cls, line: str) -> "Part":
        """Parse from input line"""
        kwargs: dict[str, int] = {}
        for entry in line[1:-1].split(","):
            key, value = entry.split("=")
            kwargs[key] = int(value)

        return cls(**kwargs)


@dataclass(frozen=True)
class Interval:
    """Right open interval representation"""

    left: int
    right: int

    def __len__(self) -> int:
        return self.right - self.left

    def split(self, mid: int, mid_to_left: bool = False) -> Iterator["Interval"]:
        """Split interval at mid

        Args:
            mid: Split point
            mid_to_left: If True, mid is included in the left interval. Otherwise,
                the right

        Yields:
            New, non-empty intervals
        """
        if mid_to_left:
            mid += 1

        for left, right in ((self.left, mid), (mid, self.right)):
            interval = Interval(left, right)
            if len(interval) > 0:
                yield interval


@dataclass
class PartCube:
    """Abstraction for a 4D-cube of parts"""

    x: Interval = Interval(1, 4_001)
    m: Interval = Interval(1, 4_001)
    a: Interval = Interval(1, 4_001)
    s: Interval = Interval(1, 4_001)

    def __getitem__(self, i: Literal["x", "m", "a", "s"]) -> Interval:
        return getattr(self, i)

    @property
    def volume(self) -> int:
        """Get cube volume"""
        return len(self.x) * len(self.m) * len(self.a) * len(self.s)

    def split(
        self, attribute: Literal["x", "m", "a", "s"], threshold: int, lower_equal: bool
    ) -> Iterator["PartCube"]:
        """Split cube along attribute axis

        Args:
            attribute: Axis along which to split
            threshold: Split point
            lower_equal: If True, split point is included in the left interval.

        Yields:
            New, non-empty cubes
        """
        for interval in self[attribute].split(threshold, mid_to_left=lower_equal):
            cube = PartCube(
                **{key: self[key] if key != attribute else interval for key in "xmas"}
            )
            if cube.volume > 0:
                yield cube


@dataclass
class Node:
    """Workflow node representation

    Attributes:
        attribue: Attribute to check
        threshold: Split point
        left: Left child node or name of the next node
        right: Right child node or name of the next node
        lower_equal: If True, split point is included in the left interval.
    """

    attribute: Literal["x", "m", "a", "s"] = "x"
    threshold: int = 0
    left: Union[str, "Node"] = ""
    right: Union[str, "Node"] = ""
    lower_equal: bool = False

    def check_part(self, part: Part) -> str:
        """Check part against workflow node

        Args:
            part: Part to check

        Returns:
            Name of the next node, "A" (accept), or "R" (reject)
        """
        attribute = part[self.attribute]
        if self.lower_equal:
            attribute -= 0.5

        if attribute < self.threshold:
            if isinstance(self.left, str):
                return self.left
            return self.left.check_part(part)

        if isinstance(self.right, str):
            return self.right
        return self.right.check_part(part)

    def subdivide_cube(self, cube: PartCube) -> Iterator[tuple[str, PartCube]]:
        """Split cube along workflow node

        Args:
            cube: Cube to split

        Yields:
            (name, new_cube), wehere name is the name of the next node, "A" (accept),
            or "R" (reject)
        """
        for new_cube, child in zip(
            cube.split(self.attribute, self.threshold, self.lower_equal),
            [self.left, self.right],
        ):
            if isinstance(child, str):
                yield child, new_cube
            else:
                yield from child.subdivide_cube(new_cube)


def workflow_from_line(line: str) -> tuple[str, Node]:
    """Parse single workflow from input line"""
    name, rest = line.split("{")

    node = start = Node()
    left = True

    for instruction in rest[:-1].split(","):
        match = re.match(r"([xmas])([<>])(\d+):(\w+)", instruction)
        if match is None:
            new_node = instruction
        else:
            new_node = Node(
                attribute=match.group(1),
                threshold=int(match.group(3)),
                lower_equal=match.group(2) == ">",
            )

            if match.group(2) == "<":
                new_node.left = match.group(4)
                new_left = False
            else:
                new_node.right = match.group(4)
                new_left = True

        if left:
            node.left = new_node
        else:
            node.right = new_node

        node = new_node
        left = new_left

    return name, start.left


@timer
def task01(input: str) -> int:
    """Solution for task 01

    Read workflow and parts and map them through the workflow.

    Args:
        input: String input

    Returns:
        Total value of all parts that are accepted by the workflow
    """
    workflow_part = True
    workflows: dict[str, Node] = {}

    total = 0
    for line in input.splitlines():
        line = line.strip()

        # Switch from reading workflows to reading parts
        if line == "":
            workflow_part = False
            continue

        # Read workflow
        if workflow_part:
            name, workflow = workflow_from_line(line)
            workflows[name] = workflow
            continue

        # Read and map parts
        part = Part.from_line(line)
        name = "in"
        while name in workflows:
            name = workflows[name].check_part(part)

        if name == "A":
            total += part.total

    return total


@timer
def task02(input: str) -> int:
    """Soluation to task 02

    Read workflow and map a cube of parts through it.

    Args:
        input: Input string

    Returns:
        Total volume of all accepted parts
    """
    workflows: dict[str, Node] = {}
    for line in input.splitlines():
        line = line.strip()

        # We don't need the parts list
        if line == "":
            break

        # Read workflow
        name, workflow = workflow_from_line(line)
        workflows[name] = workflow

    # Map cube through workflow
    # Stack instead of queue to hopefully keep the number of cubes in memory low
    stack = [("in", PartCube())]
    total_volume = 0
    while stack:
        name, cube = stack.pop()

        if name in workflows:
            stack.extend(workflows[name].subdivide_cube(cube))
            continue

        if name == "A":
            total_volume += cube.volume

    return total_volume


@main.command()
def entrypoint(path: Path):
    """CLI entrypoint"""
    with open(path, "r") as f:
        input = f.read().strip()

    logger.info(f"Task01: {task01(input)}")
    logger.info(f"Task02: {task02(input)}")


if __name__ == "__main__":
    main()
