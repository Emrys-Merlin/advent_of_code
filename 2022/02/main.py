from pathlib import Path

import click

# Task 01
# A, X: Rock
# B, Y: Paper
# C, Z: Scissors

win_table = {
    "A": {"X": 3, "Y": 6, "Z": 0},
    "B": {"X": 0, "Y": 3, "Z": 6},
    "C": {"X": 6, "Y": 0, "Z": 3},
}

shape_value = {
    "X": 1,
    "Y": 2,
    "Z": 3,
}

# Task 02
# X lose
# Y draw
# Z win
shape_table = {
    "A": {"X": "Z", "Y": "X", "Z": "Y"},
    "B": {"X": "X", "Y": "Y", "Z": "Z"},
    "C": {"X": "Y", "Y": "Z", "Z": "X"},
}

match_value = {
    "X": 0,
    "Y": 3,
    "Z": 6,
}


@click.command()
@click.argument("fn", type=click.Path(exists=True, dir_okay=False))
def main(fn: str):
    """Solution for day 02

    FN should point to the input file for today.

    \f
    Parameters
    ----------
    fn : str
        Path to input file
    """
    in_fn = Path(fn)

    points_task1 = 0
    points_task2 = 0
    with open(in_fn, "r") as f:
        for line in f.readlines():
            # Task 01
            enemy_shape, my_shape = line.strip().split(" ")
            points_task1 += win_table[enemy_shape][my_shape] + shape_value[my_shape]

            # Task 02
            match_result = my_shape
            my_shape = shape_table[enemy_shape][match_result]
            points_task2 += match_value[match_result] + shape_value[my_shape]

    print(f"Task 01: {points_task1}")
    print(f"Task 02: {points_task2}")


if __name__ == "__main__":
    main()
