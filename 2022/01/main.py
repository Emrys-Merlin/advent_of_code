from pathlib import Path

import click


@click.command()
@click.argument("fn", type=click.Path(exists=True, dir_okay=False))
def main(fn: str):
    """Solution for day 01

    FN should point to the input file for today.

    \f
    Parameters
    ----------
    fn : str
        Path to input file
    """
    in_fn = Path(fn)

    elves = [0]
    with open(in_fn, "r") as f:
        for line in f.readlines():
            line = line.strip()
            if len(line):
                elves[-1] += int(line)
            else:
                elves.append(0)

    elves.sort(reverse=True)
    most_calories = elves[0]
    print(f"Task01: {most_calories}")

    top3_calories = sum(elves[:3])
    print(f"Task02: {top3_calories}")


if __name__ == "__main__":
    main()
