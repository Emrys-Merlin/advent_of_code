from pathlib import Path

from loguru import logger
from typer import Typer
from utils import timer

main = Typer()


def extend_sequence(sequence: list[int]) -> tuple[int, int]:
    """Extend sequence to the left and right

    Raises:
        ValueError: Recursive deltas need to become constant,
                    if they don't, sequence is not extendable

    Returns:
        (left extension, right extension)
    """
    if min(sequence) == max(sequence):
        return sequence[-1], sequence[-1]

    if len(sequence) == 1:
        raise ValueError("Sequence must have at least two elements")

    delta_lo, delta_hi = extend_sequence(
        [hi - lo for lo, hi in zip(sequence[:-1], sequence[1:])]
    )

    return sequence[0] - delta_lo, sequence[-1] + delta_hi


@timer
def tasks(input: str) -> tuple[int, int]:
    """Extend all sequences left and right

    Add all left extensions and all right extensions
    respectively.

    Args:
        input: Input string

    Returns:
        (sum all left extensions, sum all right extensions)
    """
    res_lo = res_hi = 0
    for line in input.splitlines():
        lo, hi = extend_sequence([int(x) for x in line.split()])

        res_lo += lo
        res_hi += hi

    return res_lo, res_hi


@main.command()
def entrypoint(path: Path):
    """CLI entrypoint"""
    with open(path, "r") as f:
        input = f.read().strip()

    res_lo, res_hi = tasks(input)
    logger.info(f"Task 01: {res_hi}")
    logger.info(f"Task 02: {res_lo}")


if __name__ == "__main__":
    main()
