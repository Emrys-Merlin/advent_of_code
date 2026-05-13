import pytest
from aoc_lib.python.graph.kruskal import kruskal
from aoc_lib.rust.graph.kruskal import kruskal as kruskal_rust


def test_disconnected_graph() -> None:
    nodes = [1, 2, 3]
    edges = {(1, 2): 5}

    with pytest.raises(ValueError):
        _ = kruskal(nodes, edges)


def test_triangle() -> None:
    nodes = [1, 2, 3]
    edges = {
        (1, 2): 1,
        (2, 3): 2,
        (3, 1): 3,
    }

    result = kruskal(nodes, edges)

    assert result.total == 3
    assert len(result.mst) == 2


def test_single_node() -> None:
    nodes = [1]
    edges: dict[tuple[int, int], int] = {}

    result = kruskal(nodes, edges)

    assert result.total == 0
    assert len(result.mst) == 0


def test_no_node() -> None:
    nodes: list[int] = []
    edges: dict[tuple[int, int], int] = {}

    result = kruskal(nodes, edges)

    assert result.total == 0
    assert len(result.mst) == 0


def test_triangle_rust() -> None:
    n = 3
    edges = {
        (1, 2): 1,
        (2, 0): 2,
        (0, 1): 3,
    }

    total, mst = kruskal_rust(n, edges)

    assert total == 3
    assert len(mst) == 2
