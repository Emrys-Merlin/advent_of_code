"""Solutions on day 14.

https://adventofcode.com/2021/day/14
"""
from collections import Counter
from pathlib import Path
from typing import Dict, Union

import click
from tqdm import tqdm


def polymerize_naive(
    polymer: str, insertion_rules: Dict[str, str], n_iterations: int
) -> int:
    """Compute polymerization naively.

    Polymer is kept as a string and new monomers are added at the
    correct places.

    """
    for _ in tqdm(range(n_iterations)):
        new_polymer = []
        for i in range(len(polymer)):
            pair = polymer[i : (i + 2)]

            new_polymer.append(pair[0])
            new_polymer.append(insertion_rules.get(pair, ""))

        polymer = "".join(new_polymer)

    counter = Counter(polymer)
    return max(counter.values()) - min(counter.values())


def task02(polymer: str, insertion_rules: Dict[str, str], n_iterations: int) -> int:
    """Compute polymerization using pair counter.

    Encode polymer as a dict with pairs as keys
    and occurences of the pairs as values.
    In the end we have to split the pairs to count
    occurences of the monomers. For that we have to take
    care of possible boundary effects (first and last monomer
    in starting polymer).

    :param polymer: Input polymer
    :param insertion_rules: How to substitute pairs
    :param n_iterations: Number of polymerization steps
    :returns: max(monomer) - min(monomer)
    """
    polymer = Counter(["".join(pair) for pair in zip(polymer[:-1], polymer[1:])])

    for _ in tqdm(range(n_iterations)):
        new_polymer = {}
        for pair, n_occurence in polymer.items():
            if pair not in insertion_rules:
                new_polymer[pair] = new_polymer.get(pair, 0) + n_occurence
                continue

            insert = insertion_rules[pair]
            left = pair[0] + insert
            right = insert + pair[1]
            new_polymer[left] = new_polymer.get(left, 0) + n_occurence
            new_polymer[right] = new_polymer.get(right, 0) + n_occurence

        polymer = new_polymer

    # Count monomers on left and right side of pairs
    # The two numbers should be the same except for boundary effects
    counter_left = {}
    counter_right = {}
    for pair, n_occurence in polymer.items():
        counter_left[pair[0]] = counter_left.get(pair[0], 0) + n_occurence
        counter_right[pair[1]] = counter_right.get(pair[1], 0) + n_occurence

    # Take care of the boundary monomers
    counter = {}
    for monomer in set(counter_left.keys()).union(counter_right.keys()):
        counter[monomer] = max(
            counter_left.get(monomer, 0), counter_right.get(monomer, 0)
        )

    return max(counter.values()) - min(counter.values())


@click.command()
@click.argument("path", type=click.Path())
@click.argument("n_iterations", type=click.INT)
def main(path: Union[str, Path], n_iterations: int):
    """Solve day 14 tasks.

    Read input from PATH and the number of steps to comput
    from N_ITERATIONS. Print the solution.
    \f

    :param path: Path to the input file
    :param n_iterations: Number of polymerization steps
    """
    path = Path(path)

    first_half = True
    insertion_rules = {}
    with open(path, "r") as f:
        for line in f.readlines():
            line = line.strip()
            if len(line) == 0:
                first_half = False
                continue
            if first_half:
                polymer = line
            else:
                k, _, v = line.split()
                insertion_rules[k] = v

    print(f"{len(polymer)=}")
    print(f"{len(insertion_rules)=}")
    print(f"{polymer=}")

    print(f"\nPolymerization result after {n_iterations} steps")
    polymerization_result = task02(polymer, insertion_rules, n_iterations)
    print(f"{polymerization_result=}")


if __name__ == "__main__":
    main()
