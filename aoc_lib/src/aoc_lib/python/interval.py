from collections.abc import Iterable
from typing import Self, final, override

from aoc_lib.interface.interval import AbstractInterval


@final
class Interval:
    def __init__(self, lo: int, hi: int) -> None:
        if hi < lo:
            raise ValueError(f"Message {lo=} needs to be smaller than {hi=}.")

        self.lo = lo
        self.hi = hi

    @classmethod
    def from_closed(cls, lo: int, hi: int) -> Self:
        return cls(lo, hi + 1)

    def to_tuple(self) -> tuple[int, int]:
        return (self.lo, self.hi)

    def __len__(self) -> int:
        return self.hi - self.lo

    def __contains__(self, x: object) -> bool:
        match x:
            case int():
                return self.lo <= x < self.hi
            case Interval(lo=lo, hi=hi):
                return (self.lo <= lo) and (hi <= self.hi)
            case AbstractInterval(lo=lo, hi=hi):
                return (self.lo <= lo) and (hi <= self.hi)
            case _:
                return False

    def __lt__(self, x: Self, /) -> bool:
        return self.to_tuple() < x.to_tuple()

    @override
    def __eq__(self, x: object) -> bool:
        if not isinstance(x, Interval):
            return False

        return self.to_tuple() == x.to_tuple()

    @override
    def __hash__(self) -> int:
        return hash(self.to_tuple())

    @override
    def __str__(self) -> str:
        return f"Interval({self.lo}, {self.hi})"

    @override
    def __repr__(self) -> str:
        return str(self)


def merge_intervals[I: AbstractInterval](intervals: Iterable[I]) -> list[I]:
    return merge_sorted_intervals(sorted(intervals))


def merge_sorted_intervals[I: AbstractInterval](
    intervals: Iterable[I],
) -> list[I]:
    result: list[I] = []
    current: I | None = None

    for interval in intervals:
        if current is None:
            current = interval
            continue

        if (interval.lo not in current) and (current.hi != interval.lo):
            result.append(current)
            current = interval
            continue

        current = type(current)(current.lo, max(current.hi, interval.hi))

    if current is not None:
        result.append(current)

    return result
