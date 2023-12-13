from pathlib import Path

from day13 import tasks
from pytest import fixture


@fixture
def input1() -> str:
    path = Path(__file__).parent.parent / "examples/day13_1.txt"

    with open(path, "r") as f:
        return f.read().strip()


def test_tasks(input1: str):
    assert tasks(input1) == (405, 400)
