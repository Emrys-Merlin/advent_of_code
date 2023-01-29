from pathlib import Path

import click

DIGIT_DICT: dict[str, int] = {
    "2": 2,
    "1": 1,
    "0": 0,
    "-": -1,
    "=": -2,
}


def decode(snafu: str, base: int = 5) -> int:
    """Decode snafu to base 10

    Parameters
    ----------
    snafu : str
        SNAFU number
    base : int, optional
        The base, by default 5

    Returns
    -------
    int
        Base 10 representation
    """
    number = 0
    for c in snafu:
        number = number * base + DIGIT_DICT[c]
    return number


def encode(number: int, base: int = 5) -> str:
    """Encode base 10 to snafu

    Parameters
    ----------
    number : int
        Base 10 number
    base : int, optional
        Base for SNAFU, by default 5

    Returns
    -------
    str
        SNAFU number
    """
    rev_snafu: list[str] = []
    while number != 0:
        residue = number % base
        number = number // 5
        if residue < 3:
            rev_snafu.append(str(residue))
            continue

        number += 1
        if residue == 3:
            rev_snafu.append("=")
        else:
            rev_snafu.append("-")

    return "".join(reversed(rev_snafu))


def total_fuel(input: str) -> int:
    """Total fuel consumed by the balloons

    Parameters
    ----------
    input : str
        SNAFU input numbers (one per line)

    Returns
    -------
    int
        Total fuel (in base 10)
    """
    return sum(decode(line) for line in input.split("\n") if len(line.strip()) != 0)


def encoded_total_fuel(input: str) -> str:
    """Total fuel consumed by the ballons in SNAFU

    Parameters
    ----------
    input : str
        SNAFU input numbers (one per line)

    Returns
    -------
    str
        Total fuel (in SNAFU)
    """
    return encode(total_fuel(input))


@click.command
@click.argument("fn", type=click.Path(exists=True, dir_okay=False))
def main(fn: str):
    """Solution for day 25

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

    total = encoded_total_fuel(input)
    print(f"Task01: {total}")


if __name__ == "__main__":
    main()
