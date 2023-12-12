from pathlib import Path

from loguru import logger
from tqdm import tqdm
from typer import Typer
from utils import timer

main = Typer()


def combinations(symbols: str, symbols_ref: str, active_group=False) -> int:
    if len(symbols) == 0 or len(symbols_ref) == 0:
        if len(symbols) == 0:
            return int(len(symbols_ref) == 0)

        return int({".", "?"}.issuperset(set(symbols)))

    if symbols_ref[-1] == ".":
        if symbols[-1] == "#":
            return 0

        return combinations(symbols[:-1], symbols_ref[:-1], active_group=False)

    res = 0
    if not active_group and symbols[-1] in {".", "?"}:
        res = combinations(symbols[:-1], symbols_ref, active_group=False)

    if symbols[-1] in {"#", "?"}:
        res += combinations(symbols[:-1], symbols_ref[:-1], active_group=True)

    return res


@timer
def task01(input: str) -> int:
    res = 0
    for line in input.splitlines():
        symbols, groups_str = line.split()
        symbols_ref = ".".join("#" * int(group) for group in groups_str.split(","))

        comb = combinations(symbols, symbols_ref)
        res += comb

    return res


@timer
def task02(input: str) -> int:
    res = 0
    for line in tqdm(input.splitlines()):
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
