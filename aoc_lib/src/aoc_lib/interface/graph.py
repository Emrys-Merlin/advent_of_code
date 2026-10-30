from collections.abc import Hashable, Mapping, Sequence
from typing import Protocol, NamedTuple

type Edge[T: Hashable] = tuple[T, T]


class KruskalResult[Node: Hashable](NamedTuple):
    total: int
    mst: list[Edge[Node]]


class Kruskal(Protocol):
    def __call__[Node: Hashable](
        self,
        nodes: Sequence[Node],
        edges: Mapping[Edge[Node], int],
    ) -> KruskalResult[Node]:
        ...


class IntegralKruskal(Protocol):
    def __call__(
        self,
        n: int,
        edges: Mapping[Edge[int], int],
    ) -> KruskalResult[int]:
        ...
