from pathlib import Path

from day14 import Grid, task01
from pytest import fixture


@fixture
def input1() -> str:
    path = Path(__file__).parent.parent / "examples/day14_1.txt"

    with open(path, "r") as f:
        return f.read().strip()


@fixture
def ref() -> str:
    path = Path(__file__).parent.parent / "examples/day14_2.txt"

    with open(path, "r") as f:
        return f.read().strip()


def test_move_line():
    input = list(".O#O..OO#...O")
    output = list("O.#OOO..#O...")
    Grid.move_line(input)
    assert input == output


def test_spin_cycle(input1: str, ref: str):
    grid = Grid.from_input(input1)
    grid.spin_cycle()
    assert str(grid) == ref


def test_task01(input1: str):
    assert task01(input1) == 136
