import re
from collections import deque
from dataclasses import dataclass
from math import lcm, prod
from pathlib import Path
from typing import Iterator

import click


@dataclass
class Monkey:
    """Representation of a monky

    Returns
    -------
    Monkey
        _description_

    Yields
    ------
    tuple[int, int]
        destination monkey, worry
    """

    items: deque[int]
    formula: str
    divisor: int
    throwto: dict[bool, int]
    n_inspected: int = 0
    reduce_worry: bool = True

    def __len__(self) -> int:
        """Current amount of items

        Returns
        -------
        int
            Current amount of items
        """
        return len(self.items)

    def append(self, item: int):
        """Append new item

        Parameters
        ----------
        item : int
            Worry level of item
        """
        self.items.append(item)

    def get_destination(self) -> Iterator[tuple[int, int]]:
        """Generate destinations for the current items

        Yields
        ------
        Iterator[tuple[int, int]]
            destination, worry
        """
        while len(self.items) != 0:
            self.n_inspected += 1
            worry = self.items.popleft()
            # You should probably never do that, but... xD
            worry = eval(self.formula.replace("old", str(worry)))
            if self.reduce_worry:
                worry //= 3
            yield self.throwto[worry % self.divisor == 0], worry


class Troop:
    PATTERN = r"""Monkey (\d+):
  Starting items: (.*)
  Operation: new = (.*)
  Test: divisible by (\d+)
    If true: throw to monkey (\d+)
    If false: throw to monkey (\d+)
"""

    def __init__(self, input: str, reduce_worry: bool = True) -> None:
        """Create the troop of monkeys

        I looked it up. Troop seems to be the correct term.

        Parameters
        ----------
        input : str
            Input representation of the puzzle
        reduce_worry : bool, optional
            Whether the worry is divided by 3 for the monkeys, by default True
        """
        self.monkeys: dict[int, Monkey] = {}
        divisors: list[int] = []
        for (
            monkey,
            items_str,
            formula,
            divisor,
            monkey_true,
            monkey_false,
        ) in re.findall(self.PATTERN, input):
            items = deque([int(item.strip()) for item in items_str.split(",")])

            self.monkeys[int(monkey)] = Monkey(
                items=items,
                formula=formula,
                divisor=int(divisor),
                throwto={True: int(monkey_true), False: int(monkey_false)},
                reduce_worry=reduce_worry,
            )
            divisors.append(int(divisor))

        # We don't neet the absolute worry level. Only up to the least common
        # multiple of all divisors involved
        self.lcm = lcm(*divisors)

    def play_catch(self, n_rounds: int = 20) -> int:
        """Play the catch game with the monkeys

        Parameters
        ----------
        n_rounds : int, optional
            How many rounds to play, by default 20

        Returns
        -------
        int
            The number of inspected items by the top two monkeys multiplied
        """

        for _ in range(1, n_rounds + 1):
            for monkey in self.monkeys.values():
                for dest, worry in monkey.get_destination():
                    # Keep worry levels under control
                    worry %= self.lcm
                    self.monkeys[dest].append(worry)

        return prod(
            sorted(
                (monkey.n_inspected for monkey in self.monkeys.values()), reverse=True
            )[:2]
        )

    def __str__(self) -> str:
        """Print current state of the troop for debugging

        Returns
        -------
        str
            String representation of the troop
        """
        return (
            "\n".join(
                f"Monkey {i:03d}:\n  Items:"
                + ", ".join(str(item) for item in monkey.items)
                + f"\n  Inspected: {monkey.n_inspected}"
                for i, monkey in self.monkeys.items()
            )
            + "\n"
        )


@click.command()
@click.argument("fn", type=click.Path(exists=True, dir_okay=False))
def main(fn: str):
    """Solution for day 11

    FN should point to the input file for today.

    \f
    Parameters
    ----------
    fn : str
        Path to input file
    """
    in_fn = Path(fn)

    with open(in_fn, "r") as f:
        input = f.read()

    troop = Troop(input)
    result = troop.play_catch(n_rounds=20)
    print(f"Task 01: {result}")

    troop = Troop(input, reduce_worry=False)
    result = troop.play_catch(n_rounds=10_000)
    print(f"Task 02: {result}")


if __name__ == "__main__":
    main()
