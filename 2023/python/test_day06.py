from pathlib import Path

from day06 import parse_input01, parse_input02, winning_product
from pytest import fixture


@fixture
def input() -> str:
    path = Path(__file__).parent.parent / "examples/day06_1.txt"

    with open(path, "r") as f:
        return f.read().strip()


def test_task01(input):
    races = parse_input01(input)
    assert winning_product(races) == 288


def test_task02(input):
    races = parse_input02(input)
    assert winning_product(races) == 71503
