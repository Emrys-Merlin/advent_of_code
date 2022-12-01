"""Solutions on day 12.

https://adventofcode.com/2021/day/12
"""
from collections import defaultdict
from pathlib import Path
from typing import Dict, List, Tuple, Union

import click


def tasks(neighbors: Dict[str, List[str]]) -> Tuple[int, int]:
    """Compute the number of paths (for both task 01 and 02).

    :param neighbors: Adjacency list of all nodes
    :returns: Tuple with number of paths with small caves visited only once and
    the additional paths if a single small cave is allowed to be visited twice.
    """
    stack = [(["start"], False)]

    counter = 0
    counter_two_small_nodes = 0
    while len(stack) != 0:
        path, small_node_visited_twice = stack.pop()

        current_node = path[-1]

        if current_node == "end":
            if small_node_visited_twice:
                counter_two_small_nodes += 1
            else:
                counter += 1
            continue

        for node in neighbors[current_node]:
            if node == "start":
                continue

            this_small_node_already_visited = node.islower() and node in path
            if this_small_node_already_visited and small_node_visited_twice:
                continue

            stack.append(
                (
                    path + [node],
                    small_node_visited_twice or this_small_node_already_visited,
                )
            )

    return counter, counter_two_small_nodes


@click.command()
@click.argument("path", type=click.Path())
def main(path: Union[str, Path]):
    """Solve day 12 tasks.

    Read input from PATH and prints the solutions.
    \f

    :param path: Path to the input file
    """
    path = Path(path)

    neighbors = defaultdict(list)
    with open(path, "r") as f:
        for line in f.readlines():
            node1, node2 = line.strip().split("-")
            neighbors[node1].append(node2)
            neighbors[node2].append(node1)

    print(f"Number of nodes: {len(neighbors)}")

    n_paths, n_paths_second = tasks(neighbors)

    print("\nTask01")
    print(f"{n_paths=}")

    print("\nTask02")
    n_total = n_paths + n_paths_second
    print(f"{n_total=}")


if __name__ == "__main__":
    main()
