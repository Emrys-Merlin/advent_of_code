from pathlib import Path

from day19 import task01, task02
from pytest import fixture


@fixture
def input1() -> str:
    path = Path(__file__).parent.parent / "examples/day19_1.txt"

    with open(path, "r") as f:
        return f.read().strip()


def test_task01(input1: str):
    assert task01(input1) == 19114


def test_task02(input1: str):
    assert task02(input1) == 167409079868000
