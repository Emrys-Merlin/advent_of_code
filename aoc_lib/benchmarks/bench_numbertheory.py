from types import ModuleType
import pytest
from pytest_benchmark.fixture import BenchmarkFixture

IMPL_MODULE = "numbertheory"


CASES = {
    "small": (12, 8),
    "medium": (123456789, 987654321),
    "large": (2**31 - 1, 2**30 + 1),
}


@pytest.mark.parametrize(
    "m,n",
    CASES.values(),
    ids=CASES.keys(),
)
def test_gcd(
    benchmark: BenchmarkFixture,
    impl: ModuleType,
    m: int,
    n: int,
) -> None:
    benchmark.group = f"gcd[{m}, {n}]"
    benchmark(impl.gcd, m, n)  # pyright: ignore[reportAny]


@pytest.mark.parametrize(
    "m,n",
    CASES.values(),
    ids=CASES.keys(),
)
def test_eea(
    benchmark: BenchmarkFixture,
    impl: ModuleType,
    m: int,
    n: int,
) -> None:
    benchmark.group = f"eea[{m}, {n}]"
    benchmark(impl.extended_euclidean_algorithm, m, n)  # pyright: ignore[reportAny]
