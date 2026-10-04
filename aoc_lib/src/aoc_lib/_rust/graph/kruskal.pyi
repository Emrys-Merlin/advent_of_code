from collections.abc import Mapping

type Edge = tuple[int, int]

def kruskal(n: int, edges: Mapping[Edge, int]) -> tuple[int, list[Edge]]: ...
