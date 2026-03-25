import pytest
from pytest_benchmark.fixture import BenchmarkFixture
import aoc_lib.numbertheory as pnt
import aoc_lib.rust.numbertheory as rnt


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
    benchmark(pnt.gcd, m, n)


@pytest.mark.parametrize("m,n", CASES)
def test_eea_python(
    benchmark: BenchmarkFixture,
    m: int,
    n: int,
) -> None:
    benchmark(pnt.extended_euclidean_algorithm, m, n)


@pytest.mark.parametrize("m,n", CASES)
def test_gcd_rust(
    benchmark: BenchmarkFixture,
    m: int,
    n: int,
) -> None:
    benchmark(rnt.gcd, m, n)


@pytest.mark.parametrize("m,n", CASES)
def test_eea_rust(
    benchmark: BenchmarkFixture,
    m: int,
    n: int,
) -> None:
    benchmark(rnt.extended_euclidean_algorithm, m, n)
