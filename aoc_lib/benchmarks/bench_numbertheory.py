import pytest
from pytest_benchmark.fixture import BenchmarkFixture
from aoc_lib.numbertheory import gcd, extended_euclidean_algorithm

CASES = [
    (12, 8),
    (123456789, 987654321),
    (2**31 - 1, 2**30 + 1),
]


@pytest.mark.parametrize("m,n", CASES)
def test_gcd_python(
    benchmark: BenchmarkFixture,
    m: int,
    n: int,
) -> None:
    benchmark(gcd, m, n)


@pytest.mark.parametrize("m,n", CASES)
def test_eea_python(
    benchmark: BenchmarkFixture,
    m: int,
    n: int,
) -> None:
    benchmark(extended_euclidean_algorithm, m, n)
