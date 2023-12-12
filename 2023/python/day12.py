from functools import lru_cache
from pathlib import Path

from loguru import logger
from typer import Typer
from utils import timer

main = Typer()


@lru_cache(maxsize=None)
def combinations(symbols: str, symbols_ref: str, active_group=False) -> int:
    """Compute how many combinations the ref symbols fit into the symbols

    Args:
        symbols: Symbols with ?
        symbols_ref: Reference groups #-groups separated by a single .
        active_group: True if we are currently in an active group. Defaults to False.

    Returns:
        Number of combinations
    """
    # Termination condition
    if len(symbols) == 0 or len(symbols_ref) == 0:
        # If there are no more symbols, the reference must be empty
        if len(symbols) == 0:
            return int(len(symbols_ref) == 0)

        # If the reference is empty, the symbols might be non-empty,
        # but must not contain #
        return int({".", "?"}.issuperset(set(symbols)))

    # If we have a reference separator, we must have a symbol separator
    if symbols_ref[-1] == ".":
        if symbols[-1] == "#":
            return 0

        # We have a . or a ? in the symbols and can continue
        return combinations(symbols[:-1], symbols_ref[:-1], active_group=False)

    # Now we know that symbols_ref[-1] == "#"
    res = 0
    # If we are not in the middle of a group, we can treat remove . and ?
    # from symbols (i.e., treating ? as .)
    # This ends a group
    if not active_group and symbols[-1] in {".", "?"}:
        res = combinations(symbols[:-1], symbols_ref, active_group=False)

    # We can always treat ? as # and remove the # from symbols
    # and ref
    if symbols[-1] in {"#", "?"}:
        res += combinations(symbols[:-1], symbols_ref[:-1], active_group=True)

    return res


@timer
def task01(input: str) -> int:
    """Parse input for task01 and compute combinations

    Args:
        input: Input str

    Returns:
        number of combinations
    """
    res = 0
    for line in input.splitlines():
        symbols, groups_str = line.split()
        symbols_ref = ".".join("#" * int(group) for group in groups_str.split(","))

        comb = combinations(symbols, symbols_ref)
        res += comb

    return res


@timer
def task02(input: str) -> int:
    """Parse input for task02 and compute combinations

    Args:
        input: Input str

    Returns:
        number of combinations
    """
    res = 0
    for line in input.splitlines():
        symbols, group_str = line.split()
        symbols = "?".join(symbols for _ in range(5))
        group_str = ",".join(group_str for _ in range(5))

        symbols_ref = ".".join("#" * int(group) for group in group_str.split(","))

        comb = combinations(symbols, symbols_ref)
        res += comb

    return res


@main.command()
def entrypoint(path: Path):
    """Entrypoint CLI"""
    with open(path, "r") as f:
        input = f.read().strip()

    logger.info(f"Task 01: {task01(input)}")
    logger.info(f"Task 02: {task02(input)}")


if __name__ == "__main__":
    main()
