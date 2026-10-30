from typing import cast
import pytest
from itertools import product
from types import ModuleType
from pytest_benchmark.fixture import BenchmarkFixture
from random import seed, randint, getstate, setstate

from aoc_lib.interface.graph import Edge

IMPL_MODULE = "graph.kruskal"


@pytest.fixture(params=[6, 40, 1_000], ids=["small", "medium", "large"])
def n_nodes(request: pytest.FixtureRequest) -> int:
    return cast(int, request.param)


@pytest.fixture()
def integral_edges(n_nodes: int) -> dict[Edge[int], int]:
    state = getstate()
    seed(5694)
    edges = {
        (i, j): randint(0, 10) for i, j in product(range(n_nodes), repeat=2) if i != j
    }
    setstate(state)

    return edges


def test_kruskal(
    benchmark: BenchmarkFixture,
    impl: ModuleType,
    n_nodes: int,
    integral_edges: dict[Edge[int], int],
) -> None:
    nodes = list(range(n_nodes))

    benchmark.group = "kruskal"
    benchmark(
        impl.kruskal,  # pyright: ignore[reportAny]
        nodes,
        integral_edges,
    )


def test_integral_kruskal(
    benchmark: BenchmarkFixture,
    impl: ModuleType,
    n_nodes: int,
    integral_edges: dict[Edge[int], int],
) -> None:
    benchmark.group = "integral_kruskal"
    benchmark(
        impl.integral_kruskal,  # pyright: ignore[reportAny]
        n_nodes,
        integral_edges,
    )
