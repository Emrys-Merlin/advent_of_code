"""Solution to task 01 on day 01."""
import click
from typing import List, Union
from pathlib import Path


def count_sum_increases(a: List[int], n: int = 3) -> int:
    """Count consecutive increases of sums."""
    result = 0
    for pop, push in zip(a[:-n], a[n:]):
        result += (push > pop)

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

    result = count_sum_increases(a, 1)
    print(f'Task 1 result: {result}')

    result = count_sum_increases(a)
    print(f'Task 2 result: {result}')


if __name__ == '__main__':
    main()
