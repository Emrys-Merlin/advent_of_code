from pathlib import Path

from day02 import MaxDraw, sum_valid_games, total_power
from pytest import fixture


@fixture
def input() -> str:
    path = Path(__file__).parent.parent / "examples/day02_1.txt"

    with open(path, "r") as f:
        return f.read().strip()


def test_parser():
    """Check if parsing works"""
    line = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"

    draw = MaxDraw.from_line(line)

    assert draw.id == 1
    assert draw.red == 4
    assert draw.green == 2
    assert draw.blue == 6


def test_task01(input: str):
    assert sum_valid_games(input) == 8


def test_task02(input: str):
    assert total_power(input) == 2286
