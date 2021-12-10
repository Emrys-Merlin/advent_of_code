"""Solutions on day 10.

https://adventofcode.com/2021/day/10
"""
from pathlib import Path
from typing import Tuple, Union

import click

POINTS_01 = {
    ')': 3,
    ']': 57,
    '}': 1197,
    '>': 25137
}

POINTS_02 = {
    '(': 1,
    '[': 2,
    '{': 3,
    '<': 4
}

MAPPING = {
    ')': '(',
    ']': '[',
    '}': '{',
    '>': '<'
}


def analyze_line(line: str) -> Tuple[int, int]:
    """Analze a line for syntax errors.

    :param line: Line with brackets
    :returns: Tuple containing points for an incorrect line (or 0) and
    score of an incomplete line (or 0)
    """
    stack = []
    opening_brackets = {'(', '[', '{', '<'}
    closing_brackets = {')', ']', '}', '>'}

    for c in line:
        if c in opening_brackets:
            stack.append(c)
            continue

        if c in closing_brackets:
            opening = stack.pop()
            if opening != MAPPING[c]:
                return POINTS_01[c], 0

    score = 0
    while len(stack) != 0:
        opening = stack.pop()
        score = 5*score + POINTS_02[opening]

    return 0, score


@click.command()
@click.argument('path', type=click.Path())
def main(path: Union[str, Path]):
    """Solve day 10 tasks.

    Read input from PATH and prints the solutions.
    \f

    :param path: Path to the input file
    """
    path = Path(path)

    points = 0
    scores = []
    with open(path, 'r') as f:
        for line in f.readlines():
            line_points, score = analyze_line(line)
            points += line_points
            if score != 0:
                scores.append(score)

    print('Task 01')
    print(f'{points=}\n')

    print('Task 02')
    mid = len(scores) // 2
    final_score = sorted(scores)[mid]
    print(f'{final_score=}')


if __name__ == '__main__':
    main()
