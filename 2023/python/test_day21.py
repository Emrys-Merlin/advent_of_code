from pathlib import Path

from day21 import Grid, task01, task02
from pytest import fixture


@fixture
def input1() -> str:
    path = Path(__file__).parent.parent / "examples/day21_1.txt"

    with open(path, "r") as f:
        return f.read().strip()


def test_task01(input1: str):
    assert task01(input1, n_steps=6) == 16


def test_neighbors_with_glued_borders():
    grid = Grid(
        [
            list("..."),
            list("..."),
            list("..."),
        ],
        (0, 0),
    )

    ref = {
        (0, 2): {(0, 0)},
        (2, 2): {(0, 0)},
        (1, 1): {(0, 0)},
        (1, 0): {(0, 1)},
    }

    res = {
        coord: fields
        for coord, fields in grid.neighbors_with_glued_borders((1, 2), {(0, 0)})
    }

    assert res == ref


def test_task02(input1: str):
    assert task02(input1, n_steps=6) == 16
    assert task02(input1, n_steps=10) == 50
    assert task02(input1, n_steps=50) == 1594
    assert task02(input1, n_steps=100) == 6536
    assert task02(input1, n_steps=500) == 167004
    # assert task02(input1, n_steps=1_000) == 668697
    # assert task02(input1, n_steps=5_000) == 16733044
