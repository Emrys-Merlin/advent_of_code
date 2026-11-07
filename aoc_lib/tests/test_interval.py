import pytest

from aoc_lib.python.interval import Interval, merge_intervals


def test_init():
    with pytest.raises(ValueError):
        _ = Interval(5, 4)


def test_length():
    length = 4
    interval = Interval(0, length)

    assert len(interval) == length


def test_contains():
    interval = Interval(0, 6)

    assert 1 in interval
    assert 0 in interval
    assert -1 not in interval
    assert 5 in interval
    assert 6 not in interval


def test_sub_interval():
    interval = Interval(0, 6)

    assert Interval(0, 2) in interval
    assert Interval(-1, 2) not in interval
    assert Interval(5, 8) not in interval
    assert Interval(8, 9) not in interval
    assert interval in interval


def test_le():
    assert Interval(0, 6) <= Interval(1, 2)
    assert Interval(0, 6) <= Interval(0, 7)
    assert Interval(0, 6) <= Interval(0, 6)


def test_eq():
    assert Interval(0, 6) == Interval(0, 6)
    assert Interval(0, 6) != Interval(0, 7)
    assert Interval(0, 6) != Interval(-1, 6)


def test_hashable():
    {Interval(0, x) for x in range(10)}


def test_sort():
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


def test_merge_intervals():
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
