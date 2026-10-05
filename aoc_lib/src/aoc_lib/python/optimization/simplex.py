from collections.abc import Sequence, Callable
from fractions import Fraction
from math import isfinite
from typing import NamedTuple, cast

from aoc_lib.interface.optimization import SimplexResult

type Numeric = float | int
type Rational = Fraction | int


def simplex(
    c: Sequence[Rational],
    aa: Sequence[Sequence[Rational]] = (),
    b: Sequence[Rational] = (),
    aa_eq: Sequence[Sequence[Rational]] = (),
    b_eq: Sequence[Rational] = (),
    bounds: Sequence[tuple[Rational | None, Rational | None]] | None = None,
    basis: Sequence[int] | None = None,
    at_upper: Sequence[int] | None = None,
) -> SimplexResult[Fraction]:
    _ = _parse(_exact_scalar, c, aa, b, aa_eq, b_eq, bounds, basis, at_upper)
    raise NotImplementedError("Not implemented yet")


def simplex_float(
    c: Sequence[Numeric],
    aa: Sequence[Sequence[Numeric]] = (),
    b: Sequence[Numeric] = (),
    aa_eq: Sequence[Sequence[Numeric]] = (),
    b_eq: Sequence[Numeric] = (),
    bounds: Sequence[tuple[Numeric | None, Numeric | None]] | None = None,
    basis: Sequence[int] | None = None,
    at_upper: Sequence[int] | None = None,
    tol: float = 1e-9,
) -> SimplexResult[float]:
    if not tol > 0:  # this also catches nan
        raise ValueError(f"tol must be positive, observed {tol}")
    _ = _parse(_float_scalar, c, aa, b, aa_eq, b_eq, bounds, basis, at_upper)
    raise NotImplementedError("Not implemented yet")


class _Problem[T](NamedTuple):
    c: list[T]
    aa: list[list[T]]
    b: list[T]
    aa_eq: list[list[T]]
    b_eq: list[T]
    bounds: list[tuple[T | None, T | None]] | None
    basis: list[int] | None
    at_upper: list[int]


def _parse[T: Fraction | float](
    scalar: Callable[[object, str], T],
    c: Sequence[object],
    aa: Sequence[Sequence[object]],
    b: Sequence[object],
    aa_eq: Sequence[Sequence[object]],
    b_eq: Sequence[object],
    bounds: Sequence[object] | None,
    basis: Sequence[object] | None,
    at_upper: Sequence[object] | None,
) -> _Problem[T]:
    n = len(c)
    parsed_c = _vector("c", c, n, scalar)
    parsed_aa = _matrix("aa", aa, n, scalar)
    parsed_b = _vector("b", b, len(parsed_aa), scalar)
    parsed_aa_eq = _matrix("aa_eq", aa_eq, n, scalar)
    parsed_b_eq = _vector("b_eq", b_eq, len(parsed_aa_eq), scalar)
    parsed_bounds = _bounds("bounds", bounds, n, scalar)
    parsed_at_upper = _at_upper("at_upper", at_upper, n)
    parsed_basis = _basis("basis", basis, len(parsed_aa) + len(parsed_aa_eq))

    if parsed_basis is not None:
        raise NotImplementedError("Passing a basis is not supported yet.")

    if len(parsed_at_upper) != 0:
        raise NotImplementedError("Passing at_upper is not supported yet.")

    if parsed_bounds is not None:
        for i, (lo, hi) in enumerate(parsed_bounds):
            if lo is None:
                raise NotImplementedError(
                    f"bounds[{i}]: free variables are not supported; "
                    + "split x = x⁺ − x⁻ with both parts nonnegative"
                )

            if (lo, hi) != (0, None):
                raise NotImplementedError(
                    f"bounds[{i}]: only (0, None) is supported yet, got {(lo, hi)}"
                )

    return _Problem(
        parsed_c,
        parsed_aa,
        parsed_b,
        parsed_aa_eq,
        parsed_b_eq,
        parsed_bounds,
        parsed_basis,
        parsed_at_upper,
    )


def _exact_scalar(v: object, where: str) -> Fraction:
    if isinstance(v, bool) or not isinstance(v, int | Fraction):
        raise TypeError(f"{where}: expected int or Fraction, got {type(v).__name__}")
    return Fraction(v)


def _float_scalar(v: object, where: str) -> float:
    if isinstance(v, bool | Fraction) or not isinstance(v, int | float):
        raise TypeError(f"{where}: expected int or float, got {type(v).__name__}")

    try:
        f = float(v)
    except OverflowError:
        raise ValueError(f"{where}: {v} does not fit in a float") from None

    if not isfinite(f):
        raise ValueError(f"{where}: entry must be finite.")

    return f


def _vector[T](
    name: str,
    values: Sequence[object],
    length: int,
    scalar: Callable[[object, str], T],
) -> list[T]:
    if length != len(values):
        raise ValueError(
            f"{name}: expected length {length}, observed length {len(values)}"
        )

    return [scalar(v, f"{name}[{i}]") for i, v in enumerate(values)]


def _matrix[T](
    name: str,
    rows: Sequence[Sequence[object]],
    ncols: int,
    scalar: Callable[[object, str], T],
) -> list[list[T]]:
    return [_vector(f"{name}[{i}]", row, ncols, scalar) for i, row in enumerate(rows)]


def _bound[T: Fraction | float](
    name: str,
    bound: object,
    scalar: Callable[[object, str], T],
) -> tuple[T | None, T | None]:
    if not isinstance(bound, tuple):
        raise TypeError(f"{name}: Bounds must be a tuple.")

    parsed_bound = cast(tuple[object, ...], bound)

    if len(parsed_bound) != 2:
        raise ValueError(f"{name}: A bound must have exactly 2 entries.")

    lo_raw, hi_raw = parsed_bound

    lo = None if lo_raw is None else scalar(lo_raw, f"{name}[lo]")
    hi = None if hi_raw is None else scalar(hi_raw, f"{name}[hi]")

    if (lo is not None) and (hi is not None) and (lo > hi):
        raise ValueError(
            f"{name}: lower bound is higher than upper bound: {lo=}, {hi=}"
        )

    return (lo, hi)


def _bounds[T: Fraction | float](
    name: str,
    bounds: Sequence[object] | None,
    length: int,
    scalar: Callable[[object, str], T],
) -> list[tuple[T | None, T | None]] | None:
    if bounds is None:
        return None
    if length != len(bounds):
        raise ValueError(
            f"{name}: expected length {length}, observed length {len(bounds)}"
        )

    return [_bound(f"{name}[{i}]", bound, scalar) for i, bound in enumerate(bounds)]


def _at_upper(
    name: str,
    at_upper: Sequence[object] | None,
    n: int,
) -> list[int]:
    if at_upper is None:
        return []

    res: list[int] = []
    for i, u in enumerate(at_upper):
        if isinstance(u, bool) or not isinstance(u, int):
            raise TypeError(f"{name}[{i}]: expected int, got {type(u).__name__}")

        if (u < 0) or (u >= n):
            raise ValueError(f"{name}[{i}]: {u} is not in [0, {n})")

        res.append(u)

    if len(res) != len(set(res)):
        raise ValueError(f"{name}: All values must be distinct.")

    return res


def _basis(
    name: str,
    basis: Sequence[object] | None,
    nrows: int,
) -> list[int] | None:
    if basis is None:
        return None

    if len(basis) != nrows:
        raise ValueError(
            f"{name}: expected length {nrows}, observed length {len(basis)}"
        )

    res: list[int] = []
    for i, b in enumerate(basis):
        if isinstance(b, bool) or not isinstance(b, int):
            raise TypeError(f"{name}[{i}]: expected int, got {type(b).__name__}")

        if b < 0:
            raise ValueError(
                f"{name}[{i}]: Entries must be non-negative, observed: {b}"
            )

        res.append(b)

    if len(res) != len(set(res)):
        raise ValueError(f"{name}: All values must be distinct.")

    return res
