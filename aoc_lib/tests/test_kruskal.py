import pytest
from aoc_lib.python.graph.kruskal import kruskal as kruskal_py
from aoc_lib.python.graph.kruskal import integral_kruskal as integral_kruskal_py
from aoc_lib.rust.graph.kruskal import kruskal as kruskal_rust
from aoc_lib.rust.graph.kruskal import integral_kruskal as integral_kruskal_rust
from aoc_lib.interface.graph import Kruskal, IntegralKruskal


@pytest.mark.parametrize(
    "kruskal",
    [
        kruskal_py,
        kruskal_rust,
    ],
)
def test_disconnected_graph(
    kruskal: Kruskal,
) -> None:
    nodes = [1, 2, 3]
    edges = {(1, 2): 5}

    with pytest.raises(ValueError):
        _ = kruskal(nodes, edges)


@pytest.mark.parametrize(
    "kruskal",
    [
        kruskal_py,
        kruskal_rust,
    ],
)
def test_triangle(
    kruskal: Kruskal,
) -> None:
    nodes = [1, 2, 3]
    edges = {
        (1, 2): 1,
        (2, 3): 2,
        (3, 1): 3,
    }

    result = kruskal(nodes, edges)

    assert result.total == 3
    assert len(result.mst) == 2


@pytest.mark.parametrize(
    "kruskal",
    [
        kruskal_py,
        kruskal_rust,
    ],
)
def test_single_node(
    kruskal: Kruskal,
) -> None:
    nodes = [1]
    edges: dict[tuple[int, int], int] = {}

    result = kruskal(nodes, edges)

    assert result.total == 0
    assert len(result.mst) == 0


@pytest.mark.parametrize(
    "kruskal",
    [
        kruskal_py,
        kruskal_rust,
    ],
)
def test_no_node(
    kruskal: Kruskal,
) -> None:
    nodes: list[int] = []
    edges: dict[tuple[int, int], int] = {}

    result = kruskal(nodes, edges)

    assert result.total == 0
    assert len(result.mst) == 0


@pytest.mark.parametrize(
    "integral_kruskal",
    [
        integral_kruskal_py,
        integral_kruskal_rust,
    ],
)
def test_triangle_rust(
    integral_kruskal: IntegralKruskal,
) -> None:
    n = 3
    edges = {
        (1, 2): 1,
        (2, 0): 2,
        (0, 1): 3,
    }

    total, mst = integral_kruskal(n, edges)

    assert total == 3
    assert len(mst) == 2
