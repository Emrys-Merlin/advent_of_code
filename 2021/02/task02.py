"""Solution to task 02 on day 02."""
import click
from pathlib import Path
from typing import Union

import numpy as np
from math import prod


@click.command()
@click.argument("path", type=click.Path())
def main(path: Union[str, Path]):
    """Compute final position of submarine.

    Read command file from PATH and follow the commands
    as given in the description of task 02. Prints the
    final position and the product of the two coordinates
    \f

    :param path: Path to the input file
    """
    path = Path(path)
    aim_dict = {
        "down": np.array([0, 1], dtype="int"),
        "up": np.array([0, -1], dtype="int"),
    }

    position = np.array([0, 0], dtype="int")
    aim = np.array([1, 0], dtype="int")

    with open(path, "r") as f:
        for line in f.readlines():
            direction, amount = line.strip().split(" ")
            amount = int(amount)

            if direction != "forward":
                aim += aim_dict[direction] * amount
            else:
                position += aim * amount

    print(f"Final position: {position}")
    result = prod(position)
    print(f"Product: {result}")


if __name__ == "__main__":
    main()
