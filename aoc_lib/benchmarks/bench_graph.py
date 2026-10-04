from itertools import product
from types import ModuleType
from pytest_benchmark.fixture import BenchmarkFixture

IMPL_MODULE = "graph.kruskal"


def test_kruskal(
    benchmark: BenchmarkFixture,
    impl: ModuleType,
) -> None:
    n = 6
    nodes = list(range(6))
    edges = {(i, j): abs(i - j) for i, j in product(range(n), repeat=2) if i != j}

    benchmark.group = "kruskal"
    benchmark(
        impl.kruskal,  # pyright: ignore[reportAny]
        nodes,
        edges,
    )


def test_integral_kruskal(
    benchmark: BenchmarkFixture,
    impl: ModuleType,
) -> None:
    n = 6
    edges = {(i, j): abs(i - j) for i, j in product(range(n), repeat=2) if i != j}

    benchmark.group = "integral_kruskal"
    benchmark(
        impl.integral_kruskal,  # pyright: ignore[reportAny]
        n,
        edges,
    )
