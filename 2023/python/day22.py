from collections import defaultdict
from dataclasses import dataclass
from heapq import heapify, heappop, heappush
from pathlib import Path

from day19 import Interval
from loguru import logger
from typer import Typer
from utils import timer

main = Typer()


@dataclass(frozen=True)
class Brick:
    """Brick abstraction

    Interval implementation reused from day19

    """

    x: Interval
    y: Interval
    z: Interval

    @property
    def height(self) -> int:
        return len(self.z)

    @classmethod
    def from_line(cls, line: str) -> "Brick":
        intervals = []
        left, right = line.split("~")
        for lo, hi in zip(left.split(","), right.split(",")):
            intervals.append(Interval(int(lo), int(hi) + 1))

        return cls(*intervals)

    def __lt__(self, other: "Brick") -> bool:
        """First, sort by brick bottom, if tied by brick top"""
        if self.z.left == other.z.left:
            return self.z.right < other.z.right
        return self.z.left < other.z.left


@dataclass
class Tower:
    """Tower abstraction

    Attributes:
        heights: 2D array of (height, brick) tuples,
        where the brick is responsible for the height
    """

    heights: list[list[tuple[int, Brick]]]

    def stack_brick(self, brick: Brick) -> set[Brick]:
        """Stack brick on top of the tower

        Find all supporting bricks by looking at the
        map in the x-y-range of the brick and taking
        the max height bricks.

        Args:
            brick: Brick to add

        Returns:
            Bricks supporting that brick
        """
        supporting_bricks: set[brick] = set()
        max_height = 0
        for x in brick.x.range():
            for y in brick.y.range():
                height, fixed_brick = self.heights[x][y]
                if height == max_height:
                    supporting_bricks.add(fixed_brick)

                if height > max_height:
                    supporting_bricks: set[Brick] = {fixed_brick}
                    max_height = height

        assert max_height <= brick.z.left

        new_height = max_height + brick.height
        for x in brick.x.range():
            for y in brick.y.range():
                self.heights[x][y] = (new_height, brick)

        return supporting_bricks

    def stack_bricks(
        self, bricks: list[Brick]
    ) -> dict[Brick, tuple[set[Brick], set[Brick]]]:
        """Stack multiple bricks ontop tower

        Bricks are sorted by bottom (if tie by top) height.
        Then each brick is added and the supporting and supported_by
        bricks are collected in a dictionary.

        Args:
            bricks: Bricks to stack (not assumed sorted)

        Returns:
            Dictionary of brick -> (supporting, supported_by) bricks
        """
        support_supported: dict[Brick, tuple[set[Brick], set[Brick]]] = defaultdict(
            lambda: (set(), set())
        )
        for brick in sorted(bricks):
            supporting_bricks = self.stack_brick(brick)
            support_supported[brick][1].update(supporting_bricks)
            for other_brick in supporting_bricks:
                support_supported[other_brick][0].add(brick)

        return support_supported


def parse_input(input: str) -> tuple[Tower, list[Brick]]:
    """Parse input into tower and bricks

    While parsing the bricks, the maximum x and y range of
    the tower is determined and saved.
    We exploit that there are no negative x- and y-coordinates
    in the input.

    Args:
        input: Input string

    Returns:
        Empty tower and list of bricks

    """
    range_x = 0
    range_y = 0
    bricks = []
    for line in input.splitlines():
        brick = Brick.from_line(line)
        bricks.append(brick)
        range_x = max(range_x, brick.x.right)
        range_y = max(range_y, brick.y.right)

    base_brick = Brick(
        Interval(0, range_x + 1), Interval(0, range_y + 1), Interval(0, 1)
    )
    heights = [[(0, base_brick)] * range_y for _ in range(range_x)]
    tower = Tower(heights)
    return tower, bricks


def chain_reaction(
    bricks: list[Brick], support_supported: dict[Brick, tuple[set[Brick], set[Brick]]]
) -> int:
    """Follow chain reaction of destroyed bricsk by destroying one brick at a time

    The supported bricks are added in a heap which is sorted by bottom (if tie by top) height.
    Then the next heap entry is tested for destruction and so forth

    We need to pass the list of real bricks separately, because the dict contains
    the base brick of the tower, which should not be considered for destruction.

    Args:
        bricks: list of real bricks
        support_supported: brick -> (supporting, supported_by) bricks

    Returns:
        Sum of all destroyed bricks by each chain reaction
    """
    counter = 0
    for brick in bricks:
        supporting, _ = support_supported[brick]

        # Keep track of which bricks are destroyed in
        # this chain reaction
        destroyed: set[Brick] = {brick}
        # Heap of bricks to be destroyed
        heap = list(supporting)
        heapify(heap)
        # If we tried that brick before
        visited: set[Brick] = set()
        while heap:
            candidate = heappop(heap)

            if candidate in visited:
                continue

            visited.add(candidate)

            cand_supporting, supported = support_supported[candidate]

            if len(supported.difference(destroyed)) == 0:
                destroyed.add(candidate)
                counter += 1
                for supp in cand_supporting:
                    heappush(heap, supp)

    return counter


@timer
def task01(input: str) -> int:
    """Solution for task 01

    Args:
        input: Input string

    Returns:
        Sum of all bricks that are supported by at least two other bricks, i.e.
        undestroyable by a single brick removal.
    """
    tower, bricks = parse_input(input)
    support_supported = tower.stack_bricks(bricks)

    return sum(
        all(len(support_supported[brick][1]) > 1 for brick in supporting)
        for supporting, _ in support_supported.values()
    )


@timer
def task02(input: str) -> int:
    """Solution for task 02

    Args:
        input: Input string

    Returns:
        Sum of all destroyed bricks considering all possible chain reactions.
    """
    tower, bricks = parse_input(input)
    support_supported = tower.stack_bricks(bricks)

    return chain_reaction(bricks, support_supported)


@main.command()
def entrypoint(path: Path):
    """CLI entrypoint"""
    with open(path, "r") as f:
        input = f.read().strip()

    logger.info(f"Task01: {task01(input)}")
    logger.info(f"Task02: {task02(input)}")


if __name__ == "__main__":
    main()
