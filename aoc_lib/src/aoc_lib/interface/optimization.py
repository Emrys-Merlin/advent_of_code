from collections.abc import Sequence
from enum import StrEnum, auto
from typing import NamedTuple, Protocol


class SimplexStatus(StrEnum):
    optimal = auto()
    infeasible = auto()
    unbounded = auto()


class SimplexResult[T](NamedTuple):
    status: SimplexStatus
    basis: list[int]
    at_upper: list[int]
    value: T | None = None
    x: list[T] | None = None
    reduced_costs: list[T] | None = None
    ray: list[T] | None = None
    ineq_dual: list[T] | None = None
    eq_dual: list[T] | None = None


class Simplex[T](Protocol):
    def __call__(
        self,
        c: Sequence[T | int],
        aa: Sequence[Sequence[T | int]] = (),
        b: Sequence[T | int] = (),
        aa_eq: Sequence[Sequence[T | int]] = (),
        b_eq: Sequence[T | int] = (),
        bounds: Sequence[tuple[T | int | None, T | int | None]] | None = None,
        basis: Sequence[int] | None = None,
        at_upper: Sequence[int] | None = None,
    ) -> SimplexResult[T]:
        ...
