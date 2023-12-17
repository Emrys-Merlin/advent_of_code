from pathlib import Path

from day17 import task01, task02
from pytest import fixture


@fixture
def input1() -> str:
    path = Path(__file__).parent.parent / "examples/day17_1.txt"

    with open(path, "r") as f:
        return f.read().strip()


@fixture
def input2() -> str:
    path = Path(__file__).parent.parent / "examples/day17_2.txt"

    with open(path, "r") as f:
        return f.read().strip()


def test_task01(input1: str):
    assert task01(input1) == 102


def test_task02(input1: str, input2: str):
    assert task02(input1) == 94
    assert task02(input2) == 71
