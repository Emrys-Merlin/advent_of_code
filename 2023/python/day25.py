from collections import defaultdict
from pathlib import Path

from loguru import logger
from typer import Typer
from utils import timer

main = Typer()


def stoer_wagner_algorithm(graph: dict[str, set[str]]) -> set[str]:
    # https://en.wikipedia.org/wiki/Stoer%E2%80%93Wagner_algorithm
    min_cut: int = sum(len(v) for v in graph.values()) // 2
    node_set = ""
    new_graph: dict[str, set[tuple[str, int]]] = {
        node: {neighbor: 1 for neighbor in neighbors}
        for node, neighbors in graph.items()
    }

    while len(new_graph) > 1:
        # Find minimum cut
        node1, node2, cut = min_cut_phase(new_graph)
        # print(node1, node2, cut, new_graph.keys())

        # Update cut
        if cut < min_cut:
            min_cut = cut
            node_set = node1

        # Merge nodes
        new_graph = merge_nodes(node1, node2, new_graph)

    return {node_set[i : i + 3] for i in range(0, len(node_set), 3)}


def min_cut_phase(graph: dict[str, set[dict[str, int]]]) -> tuple[str, str, int]:
    # https://en.wikipedia.org/wiki/Stoer%E2%80%93Wagner_algorithm#Min-cut_phase
    # Find minimum cut

    previous_v = ""
    max_v = next(iter(graph.keys()))
    visited = {max_v}

    while len(visited) < len(graph):
        previous_v = max_v
        max_weight = 0
        for v in set(graph.keys()).difference(visited):
            total_weight = sum(
                weight for neighbor, weight in graph[v].items() if neighbor in visited
            )

            if total_weight > max_weight:
                max_v = v
                max_weight = total_weight

        visited.add(max_v)

    return max_v, previous_v, max_weight


def merge_nodes(
    node1: str, node2: str, graph: dict[str, set[dict[str, int]]]
) -> dict[str, set[dict[str, int]]]:
    # https://en.wikipedia.org/wiki/Stoer%E2%80%93Wagner_algorithm#Min-cut_phase
    # Merge nodes
    # print(node1, node2)
    new_graph = graph.copy()
    # print(new_graph.keys())
    new_node = node1 + node2
    new_graph[new_node] = {}

    joint_neighbors = set(new_graph[node1].keys()).union(new_graph[node2].keys())

    for neighbor in joint_neighbors:
        if neighbor in {node1, node2}:
            continue

        weight = new_graph[node1].get(neighbor, 0) + new_graph[node2].get(neighbor, 0)
        if weight == 0:
            continue
        new_graph[new_node][neighbor] = weight

        if node1 in new_graph[neighbor]:
            del new_graph[neighbor][node1]
        if node2 in new_graph[neighbor]:
            del new_graph[neighbor][node2]

        new_graph[neighbor][new_node] = weight

    del new_graph[node1]
    del new_graph[node2]
    # print(new_graph.keys())

    return new_graph


def parse_input(input: str) -> dict[str, set[str]]:
    graph: dict[str, set[str]] = defaultdict(set)

    for line in input.splitlines():
        node, rest = line.split(": ")

        for neighbor in rest.strip().split(" "):
            graph[node].add(neighbor)
            graph[neighbor].add(node)

    return graph


@timer
def task01(data: str) -> int:
    graph = parse_input(data)

    min_cut_component = stoer_wagner_algorithm(graph)

    return len(min_cut_component) * (len(graph) - len(min_cut_component))


@timer
def task02(data: str) -> int:
    return 0


@main.command()
def entrypoint(path: Path):
    with open(path, "r") as f:
        data = f.read().strip()

    logger.info(f"Task01: {task01(data)}")
    logger.info(f"Task02: {task02(data)}")


if __name__ == "__main__":
    main()
