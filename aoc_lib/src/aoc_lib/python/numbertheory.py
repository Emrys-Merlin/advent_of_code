from typing import Annotated


def gcd(
    m: int,
    n: int,
) -> int:
    """Common greatest divisor"""
    m, n = abs(m), abs(n)

    if m < n:
        m, n = n, m

    r = m % n
    if r == 0:
        return n

    k = m // n

    return gcd(n, m - k * n)


def extended_euclidean_algorithm(
    m: int,
    n: int,
) -> tuple[
    Annotated[int, "gcd"],
    Annotated[int, "a"],
    Annotated[int, "b"],
]:
    """Extended Euclidean algorithm

    Computes g, a, and b such that
    a*m + b*n = g
    and g is the greatest common divisor of m and n.
    """
    old_r, r = m, n
    old_s, s = 1, 0
    old_t, t = 0, 1

    while r != 0:
        q = old_r // r
        old_r, r = r, old_r - q * r
        old_s, s = s, old_s - q * s
        old_t, t = t, old_t - q * t

    return old_r, old_s, old_t


def lcm(
    m: int,
    n: int,
) -> int:
    """Least common multiple."""
    g = gcd(m, n)
    return m * (n // g)
