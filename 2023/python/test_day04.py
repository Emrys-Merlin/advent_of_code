from pathlib import Path

from day04 import task01, task02
from pytest import fixture


@fixture
def input() -> str:
    path = Path(__file__).parent.parent / "examples/day04_1.txt"

    with open(path, "r") as f:
        return f.read().strip()


def test_task01(input: str):
    assert task01(input) == 13


def test_task02(input: str):
    assert task02(input) == 30
