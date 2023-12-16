from collections import defaultdict
from pathlib import Path

from loguru import logger
from typer import Typer
from utils import timer

main = Typer()


def hash(s: str) -> int:
    """Hash a string

    Args:
        s: Input string

    Returns:
        Hash value
    """
    res = 0
    for c in s:
        res += ord(c)
        res *= 17
        res %= 256

    return res


@timer
def task01(input: str) -> int:
    """Solution for task 01

    Args:
        input: Input string

    Returns:
        Sum of the hashes
    """
    return sum(hash(entry) for entry in input.split(","))


def task02(input: str) -> int:
    """Solution for task 01

    The method heavily exploits that python dicts are sorted
    by insertion order.

    Args:
        input: Input string

    Returns:
        Sum of the focusing powers
    """
    boxes = defaultdict(dict)

    for entry in input.split(","):
        if "-" in entry:
            lens_name = entry[:-1]
            focal = None
        else:
            lens_name, focal = entry.split("=")

        box_id = hash(lens_name)

        if focal is None:
            if lens_name in boxes[box_id]:
                del boxes[box_id][lens_name]
            continue

        boxes[box_id][lens_name] = int(focal)

    res = 0
    for box_id, box in boxes.items():
        for lens_id, focal in enumerate(box.values()):
            res += (1 + box_id) * (1 + lens_id) * focal

    return res


@main.command()
def entrypoint(path: Path):
    """CLI entrypoint"""
    with open(path) as f:
        input = f.read().strip()

    logger.info(f"Task 01: {task01(input)}")
    logger.info(f"Task 02: {task02(input)}")


if __name__ == "__main__":
    main()
