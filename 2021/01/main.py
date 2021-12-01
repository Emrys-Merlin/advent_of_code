"""Solution to task 01 on day 01."""
import click
from typing import List, Union
from pathlib import Path


def count_increases(a: List[int]) -> int:
    """Count consecutive increases."""
    if len(a) < 1:
        return 0

    result = 0
    for current, prev in zip(a[1:], a[:-1]):
        result += (current > prev)

    return result


def count_sum_increases(a: List[int], n: int = 3) -> int:
    """Count consecutive increases of sums."""
    new_sum = sum(a[:n])
    result = 0
    for pop, push in zip(a[:-n], a[n:]):
        old_sum = new_sum
        new_sum = old_sum + push - pop
        result += (new_sum > old_sum)

    return result


@click.command()
@click.argument('path', type=click.Path())
def main(path: Union[str, Path]):
    """Wrap everything."""
    path = Path(path)

    with open(path, 'r') as f:
        a = [
            int(line.strip())
            for line in f.readlines()
            if len(line.strip()) != 0
        ]

    result = count_increases(a)
    print(f'Task 1 result: {result}')

    result = count_sum_increases(a)
    print(f'Task 2 result: {result}')


if __name__ == '__main__':
    main()
