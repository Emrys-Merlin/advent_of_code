from pathlib import Path

from day08 import parse_input
from pytest import fixture


@fixture
def input1() -> str:
    path = Path(__file__).parent.parent / "examples/day08_1.txt"

    with open(path, "r") as f:
        return f.read().strip()


@fixture
def input2() -> str:
    path = Path(__file__).parent.parent / "examples/day08_2.txt"

    with open(path, "r") as f:
        return f.read().strip()


@fixture
def input3() -> str:
    path = Path(__file__).parent.parent / "examples/day08_3.txt"

    with open(path, "r") as f:
        return f.read().strip()


def test_task01(input1: str, input2: str):
    assert parse_input(input1).follow_path() == (2, "ZZZ")
    assert parse_input(input2).follow_path() == (6, "ZZZ")


def test_task02(input3: str):
    assert parse_input(input3).follow_ghost_path_brute_force() == 6
    assert parse_input(input3).follow_ghost_path_iterative() == 6
    assert parse_input(input3).follow_ghost_path() == 6
