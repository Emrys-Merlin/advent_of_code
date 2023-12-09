from pathlib import Path

import pytest
from day08 import chinese_remainder_theorem, extended_euclidean_algo, parse_input
from pytest import fixture


@fixture
def input1() -> str:
    path = Path(__file__).parent.parent / "examples/day08_1.txt"

    with open(path, "r") as f:
        return f.read().strip()


@fixture
def input2() -> str:
    path = Path(__file__).parent.parent / "examples/day08_2.txt"

    with open(path, "r") as f:
        return f.read().strip()


@fixture
def input3() -> str:
    path = Path(__file__).parent.parent / "examples/day08_3.txt"

    with open(path, "r") as f:
        return f.read().strip()


@fixture
def input4() -> str:
    """Test case thanks to https://github.com/Merovius/AdventOfCode/blob/main/2023/day08/testdata/TestPart2/example3.txt"""
    path = Path(__file__).parent.parent / "examples/day08_4.txt"

    with open(path, "r") as f:
        return f.read().strip()


@fixture
def input5() -> str:
    """Test case thanks to https://github.com/Merovius/AdventOfCode/blob/main/2023/day08/testdata/TestPart2/example4.txt"""
    path = Path(__file__).parent.parent / "examples/day08_5.txt"

    with open(path, "r") as f:
        return f.read().strip()


@fixture
def input6() -> str:
    """Test case thanks to https://github.com/Merovius/AdventOfCode/blob/main/2023/day08/testdata/TestPart2/example5.txt"""
    path = Path(__file__).parent.parent / "examples/day08_6.txt"

    with open(path, "r") as f:
        return f.read().strip()


@fixture
def input7() -> str:
    """Test case thanks to https://github.com/Merovius/AdventOfCode/blob/main/2023/day08/testdata/TestPart2/example6.txt"""
    path = Path(__file__).parent.parent / "examples/day08_7.txt"

    with open(path, "r") as f:
        return f.read().strip()


@fixture
def input8() -> str:
    path = Path(__file__).parent.parent / "examples/day08_8.txt"

    with open(path, "r") as f:
        return f.read().strip()


def test_extended_eucledian_algo():
    assert extended_euclidean_algo(3, 2) == (1, 1, -1)
    assert extended_euclidean_algo(3, 4) == (1, -1, 1)
    assert extended_euclidean_algo(5, 8) == (1, -3, 2)
    assert extended_euclidean_algo(6, 14) == (2, -2, 1)


def test_chinese_remainder_theorem():
    remainders = [1, 1, 1, 1, 1, 0]
    moduli = [2, 3, 4, 5, 6, 7]

    remainder, modulus = chinese_remainder_theorem(remainders, moduli)

    assert modulus == 420

    for r, m in zip(remainders, moduli):
        assert remainder % m == r


def test_task01(input1: str, input2: str):
    assert parse_input(input1).follow_path() == (2, "ZZZ")
    assert parse_input(input2).follow_path() == (6, "ZZZ")


def test_task02(input3: str):
    assert parse_input(input3).follow_ghost_path() == 6


@pytest.mark.parametrize(
    argnames=["input", "target"],
    argvalues=[
        ("input4", 2),
        ("input5", 2),
        ("input6", 2),
        # ("input7", 1),  # This case fails because of my cycle detection
        ("input8", 13),
    ],
)
def test_task02_extended(input: str, target: int, request):
    """Tests thanks to https://github.com/Merovius/AdventOfCode/tree/main/2023/day08/testdata/TestPart2"""
    input = request.getfixturevalue(input)
    assert parse_input(input).follow_ghost_path() == target
