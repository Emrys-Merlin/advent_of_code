from collections.abc import Sequence
from dataclasses import dataclass


@dataclass
class SimplexResult:
    x: list[float]
    value: float


# TODO add aa_eq and b_eq for equality constraints
# TODO implicit x >= 0 assumption?
def simplex(
    c: Sequence[float],
    aa: Sequence[Sequence[float]],
    b: Sequence[float],
) -> SimplexResult:
    """Simplex algoritm

    This algorithm solves the following optimization problem:

    min c @ x, s.t.
       aa @ x <= b
            x >= 0  # Decide if this is implicit
    """
    ...
