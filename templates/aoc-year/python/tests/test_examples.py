from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from aoc_lib.registry import Solver


def test_example(
    solver: Solver,
    content: str,
    solution: str,
) -> None:
    assert solver(content) == solution
