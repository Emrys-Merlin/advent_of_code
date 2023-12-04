from dataclasses import dataclass, field
from pathlib import Path
from typing import Iterator

from loguru import logger
from typer import Typer
from utils import timer

main = Typer()


@dataclass
class Scratchcard:
    """Represents a scratchcard

    reference and obtained are sorted in ascending order.
    """

    id: int
    reference: list[int]
    obtained: list[int]

    _reference: list[int] = field(init=False, repr=False)
    _obtained: list[int] = field(init=False, repr=False)

    @classmethod
    def from_line(cls, line: str) -> "Scratchcard":
        """Parse input line into a Scratchcard"""
        line = line.strip()
        left, right = line.split(":")

        id = int(left.split(" ")[-1])

        ref, obt = right.split("|")
        return cls(
            id=id,
            reference=[int(x) for x in ref.split()],
            obtained=[int(x) for x in obt.split()],
        )

    @property
    def reference(self):
        return self._reference

    @reference.setter
    def reference(self, value: list[int]):
        self._reference = sorted(value)

    @property
    def obtained(self):
        return self._obtained

    @obtained.setter
    def obtained(self, value: list[int]):
        self._obtained = sorted(value)

    def winning_numbers(self) -> Iterator[int]:
        """Get the winning numbers for this scratchcard"""
        r_iter = iter(self.reference)
        o_iter = iter(self.obtained)

        try:
            ref = next(r_iter)
            obt = next(o_iter)
        except StopIteration:
            return

        while True:
            try:
                if ref == obt:
                    yield ref
                    ref = next(r_iter)
                    obt = next(o_iter)
                elif ref < obt:
                    ref = next(r_iter)
                else:
                    obt = next(o_iter)
            except StopIteration:
                break

    def points(self) -> int:
        """Compute the points for this scratchcard

        0 winning numbers: 0 points
        n>0 winning numbers: 2^(n-1) points
        """
        n_winnings = len(list(self.winning_numbers()))
        if n_winnings == 0:
            return 0

        return 2 ** (n_winnings - 1)


@timer
def task01(input: str) -> int:
    """Solution for task 01"""
    return sum(Scratchcard.from_line(line).points() for line in input.splitlines())


@timer
def task02(input: str) -> int:
    """Solution for task 02"""
    scratchcards = [Scratchcard.from_line(line) for line in input.splitlines()]
    # Contains the total number of scratchcards that can be obtained with
    total_cards: list[int] = [1] * len(scratchcards)

    for scratchcard in scratchcards:
        n_winnings = len(list(scratchcard.winning_numbers()))
        for idx in range(
            scratchcard.id, min(scratchcard.id + n_winnings, len(total_cards))
        ):
            total_cards[idx] += total_cards[
                scratchcard.id - 1  # Caution: card ids are 1-indexed
            ]

    return sum(total_cards)


@main.command()
def entrypoint(path: Path):
    """Entrypoint

    Args:
        path: Path to input file
    """
    with open(path, "r") as f:
        input = f.read().strip()

    logger.info(f"Task 01: {task01(input)}")
    logger.info(f"Task 02: {task02(input)}")


if __name__ == "__main__":
    main()
