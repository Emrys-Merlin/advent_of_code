"""Solutions on day 18.

https://adventofcode.com/2021/day/18
"""
import re
from pathlib import Path
from typing import List, Optional, Tuple, Union
from copy import deepcopy

import click


class Number(object):
    """Represent a snail number."""

    def __init__(self, number: Optional[Union[List, int]] = None):
        """Initialize snail number from list.

        :param number: List representation of number
        """
        if isinstance(number, list):
            self.value = None
            self.left = Number(number[0])
            self.right = Number(number[1])
        else:
            self.value = number
            self.left = None
            self.right = None

    def tolist(self) -> Union[int, List]:
        """Transform tree to list representation."""
        if self.value is not None:
            return self.value

        return [self.left.tolist(), self.right.tolist()]

    @classmethod
    def from_string(cls: "Number", number: str) -> "Number":
        """Initialize a snail number from string.

        Based on https://www.py4u.net/discuss/174025

        :param number: String representation of number
        """
        left = r"[[]"
        right = r"[]]"
        sep = r"[,]"
        tokens = re.split(f"({left}|{right}|{sep})", number)
        stack = [[]]
        for token in tokens:
            if not token or re.match(sep, token):
                continue
            if re.match(left, token):
                stack[-1].append([])
                stack.append(stack[-1][-1])
            elif re.match(right, token):
                stack.pop()
                if not stack:
                    raise ValueError("error: opening bracket is missing")
            else:
                stack[-1].append(int(token))
        if len(stack) > 1:
            print(stack)
            raise ValueError("error: closing bracket is missing")

        number = cls(stack.pop()[0])
        number._reduce()
        return number

    def __add__(self, number: "Number") -> "Number":
        """Add two snail numbers.

        Caution: This operation changes both self and number.

        :param number: Second summand
        :returns: Sum of snail numbers
        """
        res = Number()
        res.left = self
        res.right = number
        res._reduce()
        return res

    def __repr__(self) -> str:
        """Print snail number."""
        return str(self.tolist())

    def _reduce(self):
        while True:
            has_exploded, _, _ = self._explode()
            if has_exploded:
                continue
            if self._split():
                continue
            break

    def _explode(self, depth: int = 0) -> Tuple[bool, int, int]:
        """Explode a number.

        :param depth: Count depth in tree
        :returns: has_exploded, left value of explode, right value of explode
        """
        if self.value is not None:
            return False, 0, 0
        else:
            if depth == 4:
                left = self.left.value
                right = self.right.value
                self.left = None
                self.right = None
                self.value = 0
                return True, left, right

        has_exploded, left, right = self.left._explode(depth + 1)

        if has_exploded:
            self.right._add_leftmost(right)
            return True, left, 0

        has_exploded, left, right = self.right._explode(depth + 1)

        if has_exploded:
            self.left._add_rightmost(left)
            return True, 0, right

        return False, 0, 0

    def _add_leftmost(self, value: int):
        """Add to the leftmost value in tree.

        :param value: value to add
        """
        if self.value is not None:
            self.value += value
            return

        self.left._add_leftmost(value)

    def _add_rightmost(self, value: int) -> int:
        """Add to the rightmost value.

        :param value: value to add
        """
        if self.value is not None:
            self.value += value
            return

        self.right._add_rightmost(value)

    def _split(self) -> bool:
        """Split a number.

        :returns: True if a split occured.
        """
        if self.value is not None:
            if self.value < 10:
                return False

            half = self.value // 2
            self.left = Number(half)
            self.right = Number(self.value - half)
            self.value = None
            return True

        if self.left._split():
            return True

        if self.right._split():
            return True

        return False

    def magnitude(self) -> int:
        """Compute the magnitude of a snail number.

        :returns: magnitude
        """
        if self.value is not None:
            return self.value

        return 3 * self.left.magnitude() + 2 * self.right.magnitude()


@click.command()
@click.argument("path", type=click.Path())
def main(path: Union[str, Path]):
    """Solve day 18 tasks.

    Read input from PATH and prints the solution.
    \f

    :param path: Path to the input file
    """
    path = Path(path)

    result = None
    numbers = []
    with open(path, "r") as f:
        for line in f.readlines():
            line = line.strip()
            number = Number.from_string(line)
            numbers.append(deepcopy(number))

            if result is None:
                result = number
                continue

            result += number

    print("\nTask 01")
    print(f"{result=}")
    print(f"{result.magnitude()=}")

    print("\nTask 02")
    max_magnitude = -1
    for i in range(len(numbers)):
        for j in range(len(numbers)):
            if i == j:
                continue

            mag = (deepcopy(numbers[i]) + deepcopy(numbers[j])).magnitude()
            if mag > max_magnitude:
                max_magnitude = mag

    print(f"{max_magnitude=}")


if __name__ == "__main__":
    main()
