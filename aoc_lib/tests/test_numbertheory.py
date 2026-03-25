from typing import Callable

import pytest
from aoc_lib.numbertheory import extended_euclidean_algorithm as eea_python
from aoc_lib.numbertheory import lcm as lcm_python
from aoc_lib.numbertheory import gcd as gcd_python
from aoc_lib.rust.numbertheory import gcd as gcd_rust
from aoc_lib.rust.numbertheory import extended_euclidean_algorithm as eea_rust
from aoc_lib.rust.numbertheory import lcm as lcm_rust


@pytest.mark.parametrize(
    "gcd",
    [
        gcd_python,
        gcd_rust,
    ],
)
def test_gcd(gcd: Callable[[int, int], int]) -> None:
    assert gcd(2, 4) == 2
    assert gcd(5, 14) == 1
    assert gcd(3, 3) == 3
    assert gcd(6, 8) == 2


@pytest.mark.parametrize(
    "extended_euclidean_algorithm",
    [
        eea_python,
        eea_rust,
    ],
)
def test_extended_euclidean_algorithm(
    extended_euclidean_algorithm: Callable[[int, int], tuple[int, int, int]],
) -> None:
    assert extended_euclidean_algorithm(2, 3) == (1, -1, 1)
    assert extended_euclidean_algorithm(-2, 3) == (1, 1, 1)
    assert extended_euclidean_algorithm(8, 6) == (2, 1, -1)


@pytest.mark.parametrize(
    "lcm",
    [
        lcm_python,
        lcm_rust,
    ],
)
def test_lcm(lcm: Callable[[int, int], int]) -> None:
    assert lcm(2, 3) == 6
    assert lcm(2, 4) == 4
    assert lcm(6, 8) == 24
