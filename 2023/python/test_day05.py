from pathlib import Path

import pytest
from day05 import Map, Range, join_sorted_ranges, tasks
from pytest import fixture


@fixture
def input() -> str:
    path = Path(__file__).parent.parent / "examples/day05_1.txt"

    with open(path, "r") as f:
        return f.read().strip()


@pytest.mark.parametrize(
    "input_range, output_ranges",
    [
        (Range(-2, 1), [Range(-2, 1)]),
        (Range(-1, 2), [Range(-1, 1), Range(10, 1)]),
        (Range(-1, 4), [Range(-1, 1), Range(10, 2), Range(2, 1)]),
        (Range(6, 2), [Range(21, 2)]),
        (Range(7, 2), [Range(22, 1), Range(30, 1)]),
        (Range(10, 3), [Range(32, 2), Range(12, 1)]),
        (Range(15, 2), [Range(15, 2)]),
    ],
)
def test_range_map(input_range: Range, output_ranges: list[Range]):
    entries = [
        Map(source=0, destination=10, length=2),
        Map(source=5, destination=20, length=3),
        Map(source=8, destination=30, length=4),
    ]

    assert list(input_range.map(entries)) == output_ranges


def test_join_sorted_ranges():
    input = [
        Range(0, 1),
        Range(1, 2),
        Range(5, 8),
        Range(6, 8),
        Range(30, 1),
    ]

    output = [
        Range(0, 3),
        Range(5, 9),
        Range(30, 1),
    ]

    assert join_sorted_ranges(input) == output


def test_tasks(input):
    assert tasks(input) == 35
    assert tasks(input, first=False) == 46
