from pathlib import Path

from day20 import task01  # , task02
from pytest import fixture


@fixture
def input1() -> str:
    path = Path(__file__).parent.parent / "examples/day20_1.txt"

    with open(path, "r") as f:
        return f.read().strip()


@fixture
def input2() -> str:
    path = Path(__file__).parent.parent / "examples/day20_2.txt"

    with open(path, "r") as f:
        return f.read().strip()


def test_task01(input1: str, input2: str):
    assert task01(input1) == 32_000_000
    assert task01(input2) == 1_1687_500
