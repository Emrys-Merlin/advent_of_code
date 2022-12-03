from pathlib import Path

import click


def get_double_type(content: str) -> str:
    """Return the type of the item which is in both compartments

    Parameters
    ----------
    content : str
        Content of the rucksack

    Returns
    -------
    str
        Type contained in both compartments
    """
    mid = len(content) // 2
    left = set(content[:mid])
    right = set(content[mid:])

    intersection = left.intersection(right)
    assert len(intersection) == 1
    return intersection.pop()


def get_priority(type: str) -> int:
    """Get priority of a type

    Parameters
    ----------
    type : str
        Character indicating the type

    Returns
    -------
    int
        Priority of the type
    """
    assert len(type) == 1

    if type.lower() == type:
        return ord(type) - 96
    else:
        return ord(type) - 64 + 26


def get_badge(contents: list[str]) -> str:
    """Find the badge (common item) in three rucksack contents

    Parameters
    ----------
    contents : list[str]
        the content of the three rucksacks

    Returns
    -------
    str
        Character indicating the badge of the rucksacks
    """
    badge = set(contents[0])
    for content in contents[1:]:
        badge = badge.intersection(content)

    assert len(badge) == 1
    return badge.pop()


@click.command()
@click.argument("fn", type=click.Path(exists=True, dir_okay=False))
def main(fn: str):
    """Solution for day 03

    FN should point to the input file for today.

    \f
    Parameters
    ----------
    fn : str
        Path to input file
    """
    in_fn = Path(fn)

    total_priority = 0
    group = []
    total_group_priority = 0
    with open(in_fn, "r") as f:
        for line in f.readlines():
            content = line.strip()
            priority = get_priority(get_double_type(content))
            total_priority += priority

            group.append(content)
            if len(group) == 3:
                total_group_priority += get_priority(get_badge(group))
                group = []

    print(f"Task 01: {total_priority}")
    print(f"Task 02: {total_group_priority}")


if __name__ == "__main__":
    main()
