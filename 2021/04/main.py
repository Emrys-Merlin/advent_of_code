"""Solutions on day 04.

https://adventofcode.com/2021/day/4
"""
import click
from pathlib import Path
from typing import Union, Tuple, List
from tqdm import tqdm

import numpy as np


def task01(boards: np.ndarray, order: List[int]) -> Tuple[int, int]:
    """Solve task 01.

    :param boards: np.array of shape (n_boards, board_size, board_size)
    :param order: List of draws
    :returns: (sum of unmarked fields, draw)
    """
    mask = np.zeros_like(boards)
    for draw in tqdm(order):
        mask[boards == draw] = 1

        winner = (mask.sum(axis=1) == mask.shape[1]).max(axis=-1) | (
            mask.sum(axis=2) == mask.shape[2]
        ).max(axis=-1)

        if any(winner):
            break

    idx = np.argmax(winner)
    board = boards[idx]
    mask = mask[idx]

    unmarked = board[mask == 0].sum()
    return unmarked, draw


def task02(boards: np.ndarray, order: List[int]) -> Tuple[int, int]:
    """Solve task 02.

    :param boards: np.array of shape (n_boards, board_size, board_size)
    :param order: List of draws
    :returns: (sum of unmarked fields, draw)
    """
    masks = np.ones_like(boards)

    for draw in tqdm(reversed(order), total=len(order)):
        masks[boards == draw] = 0
        looser = ~(
            (masks.sum(axis=1) == masks.shape[1]).max(axis=-1)
            | (masks.sum(axis=2) == masks.shape[2]).max(axis=-1)
        )

        if any(looser):
            idx = np.argmax(looser)
            # Make sure to mark the last box to make it a winner
            # again
            masks[boards == draw] = 1
            break

    board = boards[idx]
    mask = masks[idx]
    unmarked = board[mask == 0].sum()

    return unmarked, draw


@click.command()
@click.argument("path", type=click.Path())
def main(path: Union[str, Path]):
    """Solve day 04 tasks.

    Read input from PATH and and apply solutions
    to day 04 tasks. Prints the solution (and the factors).
    \f

    :param path: Path to the input file
    """
    path = Path(path)

    boards = []
    board = []
    with open(path, "r") as f:
        for i, line in tqdm(enumerate(f.readlines())):
            line = line.strip()

            if i == 0:
                order = [int(char) for char in line.split(",")]
                continue

            if len(line) == 0:
                if len(board) == 5:
                    boards.append(board)
                    board = []
                continue

            board.append([int(char) for char in line.split()])

    boards = np.array(boards)
    print(f"Bingo boards: {boards.shape}")
    print(f"Draws: {len(order)}")

    unmarked, draw = task01(boards, order)
    print(f"{unmarked=}\n{draw=}")
    print(f"Product: {unmarked*draw}")

    unmarked, draw = task02(boards, order)
    print(f"{unmarked=}\n{draw=}")
    print(f"Product: {unmarked*draw}")


if __name__ == "__main__":
    main()
