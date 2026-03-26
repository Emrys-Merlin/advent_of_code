from __future__ import annotations
from collections.abc import Sequence, Mapping, Hashable
from dataclasses import dataclass, field


class DUSet:
    parent: DUSet
    size: int

    def __init__(
        self,
        parent: DUSet | None = None,
        size: int = 1,
    ) -> None:
        self.parent = parent or self
        self.size = size

    def find(self) -> DUSet:
        x = self
        while x.parent != x:
            x, x.parent = x.parent, x.parent.parent

        return x

    def merge(self: DUSet, other: DUSet) -> bool:
        x = self.find()
        y = other.find()

        if x == y:
            return False

        if x.size < y.size:
            x, y = y, x

        y.parent = x
        x.size += y.size
        return True


type Edge[T: Hashable] = tuple[T, T]


@dataclass(frozen=True)
class KruskalResult[Node: Hashable]:
    total: int = 0
    mst: list[Edge[Node]] = field(default_factory=list)


def kruskal[Node: Hashable](
    nodes: Sequence[Node],
    edges: Mapping[Edge[Node], int],
) -> KruskalResult[Node]:
    """Kruskal's algorithm for minimum spanning trees.

    https://en.wikipedia.org/wiki/Kruskal%27s_algorithm

    Returns:
        The total weight of all edges in the MST and a list of the
        edges that make up the MST.

    """
    if len(nodes) <= 1:
        return KruskalResult()

    components = {node: DUSet() for node in nodes}

    mst: list[Edge[Node]] = []
    total_weight = 0
    for edge, weight in sorted(edges.items(), key=lambda x: x[1]):
        x, y = components[edge[0]], components[edge[1]]

        if x.merge(y):
            total_weight += weight
            mst.append(edge)

        if x.find().size == len(nodes):
            return KruskalResult(total_weight, mst)

    raise ValueError("The graph is disconnected.")
