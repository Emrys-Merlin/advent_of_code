from fractions import Fraction
from pathlib import Path

from day24 import Line, Plane, Vector, parse_input, task01, task02
from pytest import fixture


@fixture
def input1() -> str:
    path = Path(__file__).parent.parent / "examples/day24_1.txt"

    with open(path, "r") as f:
        return f.read().strip()


def test_cross():
    e1 = Vector(Fraction(1))
    e2 = Vector(Fraction(0), Fraction(1))
    e3 = Vector(Fraction(0), Fraction(0), Fraction(1))

    assert e1.cross(e2) == e3
    assert e2.cross(e3) == e1
    assert e3.cross(e1) == e2


def test_intersection_ignore_z():
    line1 = Line(
        Vector(Fraction(19), Fraction(13), Fraction(30)),
        Vector(Fraction(-2), Fraction(1), Fraction(-2)),
    )
    line2 = Line(
        Vector(Fraction(18), Fraction(19), Fraction(22)),
        Vector(Fraction(-1), Fraction(-1), Fraction(-2)),
    )
    line3 = Line(
        Vector(Fraction(20), Fraction(25), Fraction(34)),
        Vector(Fraction(-2), Fraction(-2), Fraction(-4)),
    )
    ref = Vector(14 + Fraction(1, 3), 15 + Fraction(1, 3))

    line1.intersection(line2, ignore_z=True) == ref
    line2.intersection(line1, ignore_z=True) == ref

    assert line2.intersection(line3, ignore_z=True) is None


def test_intersection():
    line1 = Line(
        Vector(Fraction(1), Fraction(0), Fraction(0)),
        Vector(Fraction(1), Fraction(0), Fraction(0)),
    )
    line2 = Line(
        Vector(Fraction(1), Fraction(1), Fraction(0)),
        Vector(Fraction(1), Fraction(1), Fraction(0)),
    )
    ref = Vector()

    assert line1.intersection(line2) == ref


def test_closest_point(input1: str):
    origin = Vector()
    for line in parse_input(input1):
        _, closest = line.closest_point(origin)
        assert closest in line


def test_plane_line_intersection():
    plane = Plane(Vector(5, 6, 0), Vector(0, 0, 1))
    line = Line(Vector(0, 0, 1), Vector(0, 0, 1))
    ref = Vector()

    assert plane.intersection_line(line) == ref


def test_task01(input1: str):
    assert task01(input1, lo=7, hi=27) == 2


def test_task02(input1: str):
    assert task02(input1, t_hi_min=6) == 47
