from collections.abc import Hashable, Iterable, Sized
from typing import Protocol, Self, runtime_checkable


class Sortable(Protocol):
    def __lt__(self, other: Self, /) -> bool:
        ...


@runtime_checkable
class AbstractInterval(Hashable, Sized, Sortable, Protocol):
    @property
    def lo(self) -> int:
        ...

    @property
    def hi(self) -> int:
        ...

    def __init__(self, lo: int, hi: int) -> None:
        ...

    @classmethod
    def from_closed(cls, lo: int, hi: int) -> Self:
        ...

    def __contains__(self, x: object) -> bool:
        ...


class MergeIntervals[I: AbstractInterval](Protocol):
    def __call__(self, intervals: Iterable[I]) -> list[I]:
        ...


class MergeSortedIntervals[I: AbstractInterval](Protocol):
    def __call__(self, intervals: Iterable[I]) -> list[I]:
        ...
