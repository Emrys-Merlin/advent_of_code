from pathlib import Path

from pytest import fixture

from day03 import Schematic, task01, task02


@fixture
def input() -> str:
    path = Path(__file__).parent.parent / "examples/day03_1.txt"

    with open(path, "r") as f:
        return f.read().strip()


def test_task01(input: str):
    numbers, _ = Schematic(input).extract_numbers_and_engines()
    assert task01(numbers) == 4361


def test_task02(input: str):
    _, engines = Schematic(input).extract_numbers_and_engines()
    assert task02(engines) == 467835
