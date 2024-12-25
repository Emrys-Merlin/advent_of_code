from collections import defaultdict

import graphviz


def parse_graph(data: str) -> graphviz.Digraph:
    first_part = True
    graph = graphviz.Digraph()
    outputs: dict[str, int] = {}
    inputs: dict[str, list[int]] = defaultdict(list)

    counter = 0
    for line in data.splitlines():
        line = line.strip()
        if len(line) == 0:
            first_part = False
            continue

        if first_part:
            continue

        for i, part in enumerate(line.split(" ")):
            if i == 0 or i == 2:
                inputs[part].append(counter)
            elif i == 1:
                graph.node(name=str(counter), label=part, shape="box")
            elif i == 4:
                outputs[part] = counter

        counter += 1

    for output in outputs.keys():
        if not output.startswith("z"):
            continue

        inputs[output] = [counter]
        graph.node(name=str(counter), label=output, shape="circle")

        counter += 1

    for input in inputs.keys():
        if not input.startswith("x") and not input.startswith("y"):
            continue

        outputs[input] = counter
        graph.node(name=str(counter), label=input, shape="circle")
        counter += 1

    for input, left_end_points in inputs.items():
        right_end_point = outputs[input]
        for left_end_point in left_end_points:
            graph.edge(str(left_end_point), str(right_end_point), label=input)

    return graph


def task01(data: str) -> str:
    return "not implemented"


def task02(data: str) -> str:
    graph = parse_graph(data)
    graph.view()
    return "Extract manually from graphviz"
