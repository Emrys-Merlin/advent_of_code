from pathlib import Path

from loguru import logger
from typer import Typer
from utils import timer

main = Typer()


def calibration(line: str) -> int:
    """Compute calibration

    Identify first and larg integer in the string, multiplying the first by 10
    and summing them.
    """
    if len(line.strip()) == 0:
        return 0

    first: int | None = None
    last: int = -1
    for c in line:
        if c.isdigit():
            first = first or int(c)
            last = int(c)

    return (first or 0) * 10 + last


spelling = [
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine",
]


def replace_letter_numbers(line: str) -> str:
    """Replace written numbers by digits

    oneight is interpreted as 18
    """
    res = []
    for offset in range(len(line)):
        if line[offset].isdigit():
            res.append(line[offset])
            continue

        for i, number in enumerate(spelling):
            if line[offset:].startswith(number):
                res.append(str(i + 1))

    return "".join(res)


@timer
def task01(input: str) -> int:
    """Task 01 solution"""
    return sum(calibration(line) for line in input.split("\n"))


@timer
def task02(input: str) -> int:
    """Task 02 solution"""
    return sum(calibration(replace_letter_numbers(line)) for line in input.split("\n"))


@main.command()
def entrypoint(path: Path):
    """Entrypoint reading the input

    Args:
        path: Path to input file
    """
    with open(path) as f:
        input = f.read()

    logger.info("Task 01")
    logger.info(task01(input))

    logger.info("Task 02")
    logger.info(task02(input))


if __name__ == "__main__":
    main()
