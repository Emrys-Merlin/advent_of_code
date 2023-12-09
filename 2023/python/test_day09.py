from pathlib import Path

from day09 import tasks
from pytest import fixture


@fixture
def input() -> str:
    path = Path(__file__).parent.parent / "examples/day09_1.txt"

    with open(path, "r") as f:
        return f.read().strip()


def test_tasks(input: str):
    assert tasks(input) == (2, 114)
