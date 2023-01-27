from dataclasses import dataclass
from pathlib import Path
from typing import Optional

import click
from tqdm import tqdm


@dataclass()
class Node:
    """Node of a doubly linked list

    Parameters
    ----------
    value : int
        Value of the node
    left : Node | None
        Left neighbor
    right : Node | None
        Right neighbor
    start : bool
        Whether the node is the start point of the list
    """

    value: int
    left: Optional["Node"] = None
    right: Optional["Node"] = None
    start: bool = False

    def move(self, size: int):
        """Move node value steps among list

        Parameters
        ----------
        size : int
            Length of the list
        """
        n = self.value
        if n == 0:
            return

        pos = n > 0
        n = abs(n)
        n %= size - 1

        assert self.right is not None
        assert self.left is not None
        self.left.right = self.right
        self.right.left = self.left

        if self.start:
            self.start = False
            if pos:
                self.right.start = True
            else:
                self.left.start = True

        node = self

        for _ in range(n):
            if pos:
                assert node.right is not None
                node = node.right
            else:
                assert node.left is not None
                node = node.left

        if pos:
            node.right, self.right, self.left = self, node.right, node
            assert self.right is not None
            self.right.left = self
        else:
            node.left, self.left, self.right = self, node.left, node
            assert self.left is not None
            self.left.right = self


class EncryptedFile:
    def __init__(self, data: list[int], decryption_key: int = 1) -> None:
        """Instantiate playing field

        Parameters
        ----------
        data : list[int]
            Initial value list
        decryption_key : int, optional
            decryption key (for task02), by default 1
        """

        left: Node | None = None

        self.data: list[Node] = []

        for i, value in enumerate(data):
            node = Node(value=value * decryption_key, left=left)
            self.data.append(node)

            if left is not None:
                left.right = node

            left = node

        self.data[0].left = left
        self.data[-1].right = self.data[0]
        self.data[0].start = True

    def mix(self, n_times: int = 1):
        """Perform mixing for decodign

        Parameters
        ----------
        n_times : int, optional
            How often to mix, by default 1
        """
        for _ in tqdm(range(n_times)):
            for node in self.data:
                node.move(len(self.data))

    @property
    def coordinates(self) -> int:
        """Return coordinates."""
        start = self.data[0]
        node = start.right
        data = [start]
        split = 0
        idx = 0
        while node != start:
            idx += 1
            assert node is not None
            data.append(node)
            if node.value == 0:
                split = idx
            node = node.right

        data = data[split:] + data[:split]

        return sum(data[idx % len(data)].value for idx in [1000, 2000, 3000])

    def __str__(self) -> str:
        """String representation of current order

        Mostly used for debugging

        Returns
        -------
        str
            String representation of current order
        """
        node: Node | None = None
        for node in self.data:
            if node.start:
                break

        assert node is not None
        start = node
        values = [start.value]
        node = start.right
        while node != start:
            assert node is not None
            values.append(node.value)
            assert node.right is not None
            node = node.right
        return str(values)


@click.command()
@click.argument("fn", type=click.Path(exists=True, dir_okay=False))
def main(fn: str):
    """Solution for day 20

    FN should point to the input file for today.

    \f
    Parameters
    ----------
    fn : str
        Path to input file
    """
    in_fn = Path(fn)

    data: list[int] = []
    with open(in_fn, "r") as f:
        for line in f.readlines():
            data.append(int(line.strip()))

    ef = EncryptedFile(data)
    ef.mix()
    coordinates = ef.coordinates
    print(f"Task01: {coordinates}")

    ef = EncryptedFile(data, decryption_key=811589153)
    ef.mix(n_times=10)
    coordinates = ef.coordinates
    print(f"Task02: {coordinates}")


if __name__ == "__main__":
    main()
