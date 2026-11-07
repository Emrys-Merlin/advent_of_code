from collections.abc import Iterable
from functools import total_ordering
from typing import TYPE_CHECKING, Self, final, override

if TYPE_CHECKING:
    from aoc_lib.interface.interval import AbstractInterval


@final
@total_ordering
class Interval:
    def __init__(self, lo: int, hi: int) -> None:
        if hi < lo:
            raise ValueError(f"Message {lo=} needs to be smaller than {hi=}.")

        self.lo = lo
        self.hi = hi

    @classmethod
    def from_closed(cls, lo: int, hi: int) -> Self:
        return cls(lo, hi + 1)

    def __len__(self) -> int:
        return self.hi - self.lo

    def __contains__(self, x: object) -> bool:
        if isinstance(x, int):
            return self.lo <= x < self.hi

        if isinstance(x, Interval):
            return (self.lo <= x.lo) and (x.hi <= self.hi)

        return False

    def __le__(self, x: object) -> bool:
        if not isinstance(x, Interval):
            raise NotImplementedError(
                f"Intervals are only comparable to intervals, but received {type(x)}."
            )

        if self.lo < x.lo:
            return True

        if self.lo > x.lo:
            return False

        return self.hi <= x.hi

    @override
    def __eq__(self, x: object) -> bool:
        if not isinstance(x, Interval):
            return False

        return (x.lo == self.lo) and (x.hi == self.hi)

    @override
    def __hash__(self) -> int:
        return hash((self.lo, self.hi))

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
