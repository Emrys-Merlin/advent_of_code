from itertools import product
from pytest_benchmark.fixture import BenchmarkFixture
from aoc_lib.graph.kruskal import kruskal


def test_kruskal(
    benchmark: BenchmarkFixture,
) -> None:
    n = 6
    nodes = list(range(6))
    edges = {(i, j): abs(i - j) for i, j in product(range(n), repeat=2) if i != j}

    benchmark(
        kruskal,
        nodes,
        edges,
    )
