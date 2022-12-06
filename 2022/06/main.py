from pathlib import Path

import click


def find_marker(code_txt: str, length: int = 4) -> int:
    """Find marker for distinct section

    Parameters
    ----------
    code_txt : str
        Input code
    length : int, optional
        Length of the marker, by default 4

    Returns
    -------
    int
        Position of the last character belonging to the marker (1-based indexing)
    """
    code = [c for c in code_txt]

    for i in range(length, len(code)):
        # print(code[(i - length) : i])
        marker = set(code[(i - length) : i])
        if len(marker) == length:
            return i

    return -1


@click.command()
@click.argument("fn", type=click.Path(exists=True, dir_okay=False))
def main(fn: str):
    """Solution for day 06

    FN should point to the input file for today.

    \f
    Parameters
    ----------
    fn : str
        Path to input file
    """
    in_fn = Path(fn)

    with open(in_fn, "r") as f:
        code = f.readline().strip()

    # print(code)
    offset = find_marker(code)
    print(f"Task 01: {offset}")

    offset = find_marker(code, length=14)
    print(f"Task 02: {offset}")


if __name__ == "__main__":
    main()
