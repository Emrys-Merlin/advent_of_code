from collections.abc import Hashable, Mapping, Sequence

from aoc_lib.interface.graph import KruskalResult, Edge
from aoc_lib._rust.graph.kruskal import kruskal as _kruskal
from aoc_lib._rust.graph.kruskal import indexed_kruskal


def kruskal[Node: Hashable](
    nodes: Sequence[Node],
    edges: Mapping[Edge[Node], int],
) -> KruskalResult[Node]:
    if len(nodes) <= 1:
        return KruskalResult(0, [])

    index = {node: i for i, node in enumerate(nodes)}
    total, mst = indexed_kruskal(
        len(nodes),
        index,
        edges,
    )

    return KruskalResult(total, [(nodes[u], nodes[v]) for u, v in mst])


def integral_kruskal(
    n: int,
    edges: Mapping[Edge[int], int],
) -> KruskalResult[int]:
    return KruskalResult(*_kruskal(n, edges))
