from pathlib import Path

from day18 import task01, task02
from pytest import fixture


@fixture
def input1() -> str:
    path = Path(__file__).parent.parent / "examples/day18_1.txt"

    with open(path, "r") as f:
        return f.read().strip()


def test_task01(input1: str):
    assert task01(input1) == 62


def test_task02(input1: str):
    assert task02(input1) == 952408144115
