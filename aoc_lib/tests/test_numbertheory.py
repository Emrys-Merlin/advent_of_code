from aoc_lib.numbertheory import gcd, extended_euclidean_algorithm, lcm


def test_gcd() -> None:
    assert gcd(2, 4) == 2
    assert gcd(5, 14) == 1
    assert gcd(3, 3) == 3
    assert gcd(6, 8) == 2


def test_extended_euclidean_algorithm() -> None:
    assert extended_euclidean_algorithm(2, 3) == (1, -1, 1)
    assert extended_euclidean_algorithm(-2, 3) == (1, 1, 1)
    assert extended_euclidean_algorithm(8, 6) == (2, 1, -1)


def test_lcm() -> None:
    assert lcm(2, 3) == 6
    assert lcm(2, 4) == 4
    assert lcm(6, 8) == 24
