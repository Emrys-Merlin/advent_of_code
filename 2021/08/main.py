"""Solutions on day 08.

https://adventofcode.com/2021/day/8
"""
from collections import Counter
from pathlib import Path
from typing import Dict, List, Union

import click

DIGIT_DICT = dict(
    abcefg=0,
    cf=1,
    acdeg=2,
    acdfg=3,
    bcdf=4,
    abdfg=5,
    abdefg=6,
    acf=7,
    abcdefg=8,
    abcdfg=9,
)


def decode_segments(digits: List[str]) -> Dict[str, str]:
    """Decode scrambled digits."""
    n_fives = []
    # identify unique digits and all digits with 5 segments
    for digit in digits:
        n_segments = len(digit)
        if n_segments == 2:
            one = set(digit)
        elif n_segments == 3:
            seven = set(digit)
        elif n_segments == 4:
            four = set(digit)
        elif n_segments == 5:
            n_fives.append(set(digit))

    # using 1 and 7 identify a
    a = seven.difference(one).pop()
    segment_dict = dict()
    segment_dict[a] = "a"

    # using 4 and 1 identify candidates for b and d
    b_or_d = four.difference(one)

    # With b or d identify 5
    two_or_three = []
    for n_five in n_fives:
        if b_or_d.issubset(n_five):
            five = n_five
            continue
        two_or_three.append(n_five)

    # Using 5 and 1 identify f and c
    f = five.intersection(one).pop()
    c = one.difference([f]).pop()
    segment_dict[f] = "f"
    segment_dict[c] = "c"

    # Since 2 and 3 differ only by replacing a single
    # segment, which does not affect b or d
    # we can use either one to identify b and d
    d = two_or_three[0].intersection(b_or_d).pop()
    b = b_or_d.difference([d]).pop()
    segment_dict[b] = "b"
    segment_dict[d] = "d"

    # Only e and g are left. Extract the candidates
    e_or_g = set("abcdefg").difference(segment_dict.keys())

    # 5 only contains a g. Use this to identfy e and g.
    g = five.intersection(e_or_g).pop()
    e = e_or_g.difference([g]).pop()

    segment_dict[e] = "e"
    segment_dict[g] = "g"
    return segment_dict


def transform_digit(digit: List[str], segment_dict: Dict[str, str]) -> int:
    """Transform scrambled digit to int."""
    digit = "".join(sorted([segment_dict[segment] for segment in digit]))
    return DIGIT_DICT[digit]


def transform_digits(
    digits: List[List[str]], segment_dict: Dict[str, str]
) -> List[int]:
    """Transform list of scrambled digit to list of int."""
    return [transform_digit(digit, segment_dict) for digit in digits]


def digits_to_number(digits: List[int]) -> int:
    """Interpret list of digits as base 10 number."""
    res = 0

    for digit in digits:
        res = 10 * res + digit

    return res


@click.command()
@click.argument("path", type=click.Path())
def main(path: Union[str, Path]):
    """Solve day 08 tasks.

    Read input from PATH an prints the solution.
    \f

    :param path: Path to the input file
    """
    path = Path(path)

    counter = Counter()
    numbers = []
    with open(path, "r") as f:
        for line in f.readlines():
            digits, output = line.split("|")
            digits = digits.strip().split()
            output = output.strip().split()

            segment_dict = decode_segments(digits)
            digits = transform_digits(output, segment_dict)
            counter.update(digits)
            numbers.append(digits_to_number(digits))

    print("Total digit count:")
    print(counter)

    easy_digits = [1, 4, 7, 8]
    task01_result = sum([counter[i] for i in easy_digits])
    print(f"Task 01: {task01_result}")

    print(f"Task 02: {sum(numbers)}")


if __name__ == "__main__":
    main()
