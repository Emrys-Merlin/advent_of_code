from pathlib import Path

import click


def is_subset(left_range: tuple[int, int], right_range: tuple[int, int]) -> bool:
    """Checks if one of the ranges is contained in the other.

    Parameters
    ----------
    left_range : tuple[int]
        First range to check. Endpoints are included.
    right_range : tuple[int]
        Second range to check. Endpoints are included.

    Returns
    -------
    bool
        True if one contains the other
    """
    intersection = (
        max(left_range[0], right_range[0]),
        min(left_range[1], right_range[1]),
    )
    return (intersection == left_range) or (intersection == right_range)


def overlaps(left_range: tuple[int, int], right_range: tuple[int, int]) -> bool:
    """Check if the two ranges overlap

    Parameters
    ----------
    left_range : tuple[int]
        First range to check. Endpoints are included.
    right_range : tuple[int]
        Second range to check. Endpoints are included.

    Returns
    -------
    bool
        True if there is a non-zero intersection
    """
    intersection = (
        max(left_range[0], right_range[0]),
        min(left_range[1], right_range[1]),
    )
    return intersection[0] <= intersection[1]


@click.command()
@click.argument("fn", type=click.Path(exists=True, dir_okay=False))
def main(fn: str):
    """Solution for day 04

    FN should point to the input file for today.

    \f
    Parameters
    ----------
    fn : str
        Path to input file
    """
    in_fn = Path(fn)

    contained_counter = 0
    overlap_counter = 0
    with open(in_fn, "r") as f:
        for line in f.readlines():
            left, right = line.strip().split(",")
            left_range = tuple(int(i) for i in left.split("-"))
            right_range = tuple(int(i) for i in right.split("-"))

            contained_counter += int(is_subset(left_range, right_range))
            overlap_counter += int(overlaps(left_range, right_range))

    print(f"Task 01: {contained_counter}")
    print(f"Task 02: {overlap_counter}")


if __name__ == "__main__":
    main()
