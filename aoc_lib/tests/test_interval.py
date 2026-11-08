from types import ModuleType
from typing import cast
from aoc_lib.interface.interval import AbstractInterval
import pytest

from aoc_lib.python.interval import merge_intervals
from aoc_lib.python.interval import Interval as PythonInterval
from aoc_lib.rust.interval import Interval as RustInterval

IMPL_MODULE = "interval"


@pytest.fixture
def Interval(impl: ModuleType) -> type[AbstractInterval]:
    return cast(type[AbstractInterval], impl.Interval)


def test_protocol(
    Interval: type[AbstractInterval],
):
    interval = Interval(0, 6)
    assert isinstance(interval, AbstractInterval)


def test_init(
    Interval: type[AbstractInterval],
):
    with pytest.raises(ValueError):
        _ = Interval(5, 4)


def test_length(
    Interval: type[AbstractInterval],
):
    length = 4
    interval = Interval(0, length)

    assert len(interval) == length


def test_contains(
    Interval: type[AbstractInterval],
):
    interval = Interval(0, 6)

    assert 1 in interval
    assert 0 in interval
    assert -1 not in interval
    assert 5 in interval
    assert 6 not in interval


def test_sub_interval(
    Interval: type[AbstractInterval],
):
    interval = Interval(0, 6)

    assert Interval(0, 2) in interval
    assert Interval(-1, 2) not in interval
    assert Interval(5, 8) not in interval
    assert Interval(8, 9) not in interval
    assert interval in interval


def test_lt(
    Interval: type[AbstractInterval],
):
    assert Interval(0, 6) < Interval(1, 2)
    assert Interval(0, 6) < Interval(0, 7)


def test_eq(
    Interval: type[AbstractInterval],
):
    assert Interval(0, 6) == Interval(0, 6)
    assert Interval(0, 6) != Interval(0, 7)
    assert Interval(0, 6) != Interval(-1, 6)


def test_hashable(
    Interval: type[AbstractInterval],
):
    {Interval(0, x) for x in range(10)}


def test_sort(
    Interval: type[AbstractInterval],
):
    expected = [
        Interval(0, 3),
        Interval(0, 6),
        Interval(1, 7),
        Interval(2, 3),
    ]
    input_list = [
        Interval(1, 7),
        Interval(0, 3),
        Interval(2, 3),
        Interval(0, 6),
    ]

    actual = sorted(input_list)

    assert expected == actual


def test_merge_intervals(
    Interval: type[AbstractInterval],
):
    input_list = [
        Interval(4, 7),
        Interval(1, 3),
        Interval(0, 6),
        Interval(20, 300),
        Interval(11, 12),
        Interval(9, 11),
    ]
    expected = [
        Interval(0, 7),
        Interval(9, 12),
        Interval(20, 300),
    ]

    result = merge_intervals(input_list)

    assert all(left == right for left, right in zip(result, expected, strict=True))


def test_python_rust_crossover():
    sub_interval = PythonInterval(0, 3)
    interval = RustInterval(0, 6)
    super_interval = PythonInterval(-1, 8)

    assert sub_interval in interval
    assert super_interval not in interval


def test_rust_python_crossover():
    sub_interval = RustInterval(0, 3)
    interval = PythonInterval(0, 6)
    super_interval = RustInterval(-1, 8)

    assert sub_interval in interval
    assert super_interval not in interval
