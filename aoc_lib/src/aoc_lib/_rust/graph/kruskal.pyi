from collections.abc import Mapping
from typing import Hashable

from aoc_lib.interface.graph import Edge


def kruskal(n: int, edges: Mapping[Edge[int], int]) -> tuple[int, list[Edge[int]]]: ...


def indexed_kruskal[Node: Hashable](
    n: int,
    index: dict[Node, int],
    edges: Mapping[Edge[Node], int],
) -> tuple[int, list[Edge[int]]]: ...
