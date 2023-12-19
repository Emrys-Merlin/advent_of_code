import re
from dataclasses import dataclass
from pathlib import Path
from typing import Literal, Union

from loguru import logger
from typer import Typer
from utils import timer

main = Typer()


@dataclass
class Part:
    x: int
    m: int
    a: int
    s: int

    def __getitem__(self, i: Literal["x", "m", "a", "s"]):
        return getattr(self, i)

    @property
    def total(self) -> int:
        return self.x + self.m + self.a + self.s

    @classmethod
    def from_line(cls, line: str) -> "Part":
        kwargs: dict[str, int] = {}
        for entry in line[1:-1].split(","):
            key, value = entry.split("=")
            kwargs[key] = int(value)

        return cls(**kwargs)


@dataclass
class Node:
    attribute: str = "x"
    threshold: int = 0
    left: Union[str, "Node"] = ""
    right: Union[str, "Node"] = ""
    lower_equal: bool = False

    def check_part(self, part: Part) -> str:
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


def workflow_from_line(line: str) -> tuple[str, Node]:
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
    workflow_part = True
    workflows: dict[str, Node] = {}

    total = 0
    for line in input.splitlines():
        line = line.strip()

        if line == "":
            workflow_part = False
            continue

        if workflow_part:
            name, workflow = workflow_from_line(line)
            workflows[name] = workflow
            continue

        part = Part.from_line(line)
        name = "in"
        while name in workflows:
            name = workflows[name].check_part(part)

        if name == "A":
            total += part.total

    return total


@main.command()
def entrypoint(path: Path):
    with open(path, "r") as f:
        input = f.read().strip()

    logger.info(f"Task01: {task01(input)}")


if __name__ == "__main__":
    main()
