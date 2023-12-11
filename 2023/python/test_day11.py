from pathlib import Path

from day11 import tasks
from pytest import fixture


@fixture
def input1() -> str:
    path = Path(__file__).parent.parent / "examples/day11_1.txt"

    with open(path, "r") as f:
        return f.read().strip()


def test_task01(input1: str):
    assert tasks(input1) == 374


def test_task02(input1: str):
    assert tasks(input1, expand=10) == 1030
    assert tasks(input1, expand=100) == 8410
