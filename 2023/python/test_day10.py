from pathlib import Path

import pytest
from day10 import Grid
from pytest import fixture


@fixture
def input1() -> str:
    path = Path(__file__).parent.parent / "examples/day10_1.txt"

    with open(path, "r") as f:
        return f.read().strip()


@fixture
def input2() -> str:
    path = Path(__file__).parent.parent / "examples/day10_2.txt"

    with open(path, "r") as f:
        return f.read().strip()


@fixture
def input3() -> str:
    path = Path(__file__).parent.parent / "examples/day10_3.txt"

    with open(path, "r") as f:
        return f.read().strip()


@fixture
def input4() -> str:
    path = Path(__file__).parent.parent / "examples/day10_4.txt"

    with open(path, "r") as f:
        return f.read().strip()


@fixture
def input5() -> str:
    path = Path(__file__).parent.parent / "examples/day10_5.txt"

    with open(path, "r") as f:
        return f.read().strip()


@fixture
def input6() -> str:
    path = Path(__file__).parent.parent / "examples/day10_6.txt"

    with open(path, "r") as f:
        return f.read().strip()


def test_task01(input1: str, input2: str):
    grid = Grid(input1)
    assert grid.furthest_distance() == 4

    grid = Grid(input2)
    assert grid.furthest_distance() == 8


@pytest.mark.parametrize(
    argnames=["input", "expected"],
    argvalues=[
        ("input1", 1),
        ("input2", 1),
        ("input3", 4),
        ("input4", 4),
        ("input5", 8),
        ("input6", 10),
    ],
)
def test_task02(input: str, expected: int, request):
    input = request.getfixturevalue(input)
    grid = Grid(input)
    assert grid.count_inner_component() == expected
