from collections.abc import Iterable
from functools import total_ordering
from typing import Protocol, Self, override


@total_ordering
class AbstractInterval(Protocol):
    lo: int
    hi: int

    def __init__(self, lo: int, hi: int) -> None:
        ...

    @classmethod
    def from_closed(cls, lo: int, hi: int) -> Self:
        ...

    def __len__(self) -> int:
        ...

    def __contains__(self, x: object) -> bool:
        ...

    def __le__(self, x: object) -> bool:
        ...

    @override
    def __eq__(self, x: object) -> bool:
        ...

    @override
    def __hash__(self) -> int:
        ...


class MergeIntervals[I: AbstractInterval](Protocol):
    def __call__(self, intervals: Iterable[I]) -> list[I]:
        ...


class MergeSortedIntervals[I: AbstractInterval](Protocol):
    def __call__(self, intervals: Iterable[I]) -> list[I]:
        ...
