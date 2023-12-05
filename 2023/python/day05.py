from bisect import bisect_left
from dataclasses import dataclass
from pathlib import Path
from typing import Iterator

from typer import Typer
from utils import timer

main = Typer()


@dataclass
class Map:
    """The mapping between categories"""

    source: int
    destination: int = 0
    length: int = 0

    def __lt__(self, other: "Map") -> bool:
        return self.source < other.source

    def map(self, value: int) -> int:
        """Map a value according to map.

        value needs to be in range [source, infty)

        if value < source + length, then
        new_value = value - source + destination
        Else new_value = value

        Args:
            value: value to map

        Raises:
            ValueError: I value < source

        Returns:
            new_value
        """
        if value < self.source:
            raise ValueError(
                f"{value} is not in range {self.source} - {self.source + self.length}"
            )

        if value >= self.source + self.length:
            return value

        return self.destination + value - self.source


@dataclass
class Range:
    """Seed (and other category) ranges"""

    start: int
    length: int

    def __lt__(self, other: "Range") -> bool:
        return self.start < other.start

    def map(self, maps: list["Map"]) -> Iterator["Range"]:
        """Magic function that does the hard work

        Caution! Maps needs to be sorted by source as binary
        search is applied to find the correct map for the range.

        If the range goes over map boundaries, the range is split
        and the subranges are mapped.

        Args:
            maps: Sorted list of entries (sorted by source)

        Yields:
            The mapped ranges
        """
        source = self.start
        length = self.length

        while True:
            # add 0.5 so we never hit exactly (avoids edge cases)
            # Hence, idx always points to a value smaller than or equal
            # to source (or -1, if there is none)
            idx = bisect_left(maps, source + 0.5, key=lambda x: x.source) - 1

            # Source is smaller than all maps. Yield range up until
            # the source of the first map is found
            if idx < 0:
                mp = maps[0]
                new_length = min(mp.source - source, length)
                dest = source

            # We found a map that has a smaller source
            else:
                mp = maps[idx]

                # If the current map does not cover the range,
                # we need to special case the new_length
                if mp.source + mp.length <= source:
                    # Break if we are at the last map
                    if idx == len(maps) - 1:
                        yield Range(start=source, length=length)
                        break

                    new_length = min(length, maps[idx + 1].source - source)
                    dest = source

                # We are fully within the map
                else:
                    dest = mp.map(source)
                    new_length = min(mp.source + mp.length - source, length)

            # Yield new range and update source and length
            yield Range(start=dest, length=new_length)
            if new_length == length:
                break
            source += new_length
            length -= new_length


def join_sorted_ranges(ranges: list[Range]) -> list[Range]:
    """If neighboring ranges overlap, join them.

    Assumes that ranges are sorted by start.

    Args:
        ranges: List of ranges to join.

    Returns:
        Joined ranges.
    """
    res: list[Range] = []
    current = ranges[0]
    for range in ranges[1:]:
        if range.start <= current.start + current.length:
            current.length = max(
                current.length, range.start - current.start + range.length
            )
            continue

        res.append(current)
        current = range

    res.append(current)

    return res


def get_destination_ranges(ranges: list[Range], lines: list[str]) -> list[Range]:
    """Map ranges through the different stages.

    After each stage, the ranges are sorted and joined to keep
    the number of ranges as low as possible.

    Args:
        ranges: Input ranges
        lines: Lines encoding the maps

    Returns:
        Output ranges
    """
    maps: list[Map] = []

    for line in lines[1:]:
        line = line.strip()

        # Empty line means that the next stage starts
        # We have a complete map that can be applied
        # Lastly ranges are sorted
        if len(line) == 0:
            maps.sort()
            ranges = join_sorted_ranges(
                sorted(sum((list(range.map(maps)) for range in ranges), []))
            )
            continue

        # New stage starts. Reset the maps
        if line.endswith("map:"):
            maps = []
            continue

        # One map entry
        destination, source, length = line.split()
        maps.append(
            Map(source=int(source), destination=int(destination), length=int(length))
        )

    return ranges


@timer
def tasks(input: str, first: bool = True) -> int:
    seed_line, *lines = input.splitlines()
    lines.append("")

    starts_lengths = seed_line.split(":")[1].strip().split()
    if first:
        ranges = [Range(start=int(start), length=1) for start in starts_lengths]
    else:
        ranges = [
            Range(start=int(start), length=int(length))
            for start, length in zip(starts_lengths[::2], starts_lengths[1::2])
        ]

    ranges = get_destination_ranges(ranges, lines)

    return min(ranges).start


@main.command()
def entrypoint(path: Path):
    with open(path, "r") as f:
        input = f.read().strip()

    print(f"Task 01: {tasks(input, first=True)}")
    print(f"Task 02: {tasks(input, first=False)}")


if __name__ == "__main__":
    main()
