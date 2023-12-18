from pathlib import Path

from day18 import shoelace_formula, task01, task02
from pytest import fixture


@fixture
def input1() -> str:
    path = Path(__file__).parent.parent / "examples/day18_1.txt"

    with open(path, "r") as f:
        return f.read().strip()


def test_polygon_area_formula():
    points = [(0, 0), (0, 3), (4, 3)]
    assert shoelace_formula(points) == 6

    points = [(0, 0), (1, 0), (1, 1), (0, 1)]
    assert shoelace_formula(points) == 1


def test_task01(input1: str):
    assert task01(input1) == 62


def test_task02(input1: str):
    assert task02(input1) == 952408144115
