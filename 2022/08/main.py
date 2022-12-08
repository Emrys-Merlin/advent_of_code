from collections import defaultdict
from pathlib import Path

import click


def visibility_map(grid: list[list[int]]) -> list[list[bool]]:
    """Check which trees are visible from the sides

    Parameters
    ----------
    grid : list[list[int]]
        Grid containing the tree heights

    Returns
    -------
    list[list[bool]]
        Grid with True if corresponding tree is visible
    """

    visible = []

    for row in grid:
        visible.append(line_visibility(row))

    for j in range(len(grid[0])):
        col = [grid[i][j] for i in range(len(grid))]
        for i, vis in enumerate(line_visibility(col)):
            visible[i][j] |= vis

    return visible


def line_visibility(row: list[int]) -> list[bool]:
    """Check visibility for a single line (row or column) of trees


    Parameters
    ----------
    row : list[int]
        Single line of trees

    Returns
    -------
    list[bool]
        List with True if corresponding tree is visible
    """
    visible = [False] * len(row)

    # For left save the max tree height so far
    left_max = -1
    # For right we have to keep track of the trees we have visited
    # to check whether they are shorter than the current one.
    # Key: tree_height
    # Value: list of tree_positions, seen so far (and still possibly visible)
    # at that height
    right_candidates: dict[int, list[int]] = defaultdict(list)

    for j, tree in enumerate(row):
        # Look left
        if tree > left_max:
            visible[j] = True
            left_max = tree

        # Look 'right' by removing all visibilty candidates
        # that are covered by the current tree
        for k in range(tree):
            right_candidates[k] = list()

        right_candidates[tree] = [j]

    # Set remaining 'right' candidates to visible
    for tree_cols in right_candidates.values():
        for j in tree_cols:
            visible[j] = True

    return visible


def count_visible(grid: list[list[bool]]) -> int:
    """Count visible trees

    Parameters
    ----------
    grid : list[list[bool]]
        Boolean grid encoding the visible trees as True

    Returns
    -------
    int
        Number of visible trees
    """
    return sum(sum(row) for row in grid)


def print_visibility_map(grid: list[list[bool]]) -> str:
    """Utility method to print the boolean grid for debugging

    Parameters
    ----------
    grid : list[list[bool]]
        Boolean grid of tree visibilty

    Returns
    -------
    str
        String representation of Grid
    """
    return "\n".join(
        "".join("T" if visible else "F" for visible in row) for row in grid
    )


def line_scenic_scores(row: list[int]) -> list[int]:
    """Compute the scenic score along one line (row or column)

    Parameters
    ----------
    row : list[int]
        Line (row or column) of trees

    Returns
    -------
    list[int]
        Corresponding scenic scores
    """
    # Initialize scenic map. First entry is always 0
    scenic_map = [0] + [1] * (len(row) - 1)

    # Look left: We have a stack with the highest trees so far.
    # If a new tree is considered, we pop trees from the stack until we find
    # one that is at least as high as the current tree.
    stack: list[int] = [0]

    # Look right: We save all tree heights in a dict. Once we find a tree which is
    # at least as high, we can compute the right scenic score and remove those trees from
    # tracking.
    # Key: tree height
    # Value: list of tree positions with corresponding heights (which have not been blocked
    # by a tree yet)
    open_positions: dict[int, list[int]] = defaultdict(list)
    open_positions[row[0]].append(0)

    for j, tree in enumerate(row[1:], 1):
        # Look to the left
        while len(stack) > 0:
            left_pos = stack.pop()
            left_height = row[left_pos]
            if left_height >= tree:
                stack.append(left_pos)
                break
        # If the stack is empty, we can see all the way to the left
        if len(stack) == 0:
            left_pos = 0
        scenic_map[j] *= j - left_pos
        stack.append(j)

        # Look to the right
        for height in range(tree + 1):
            for pos in open_positions[height]:
                scenic_map[pos] *= j - pos

            open_positions[height] = []

        open_positions[tree].append(j)

    # All trees that have not been blocked yet,
    # can see all the way to the right
    for positions in open_positions.values():
        for pos in positions:
            scenic_map[pos] *= len(row) - 1 - pos

    return scenic_map


def scenic_scores(grid: list[list[int]]) -> list[list[int]]:
    """Compute the scenic scores for the whole grid

    Parameters
    ----------
    grid : list[list[int]]
        Grid of tree heights

    Returns
    -------
    list[list[int]]
        Grid of scenic scores
    """
    scenic_map: list[list[int]] = []

    for row in grid:
        scenic_map.append(line_scenic_scores(row))

    for j in range(len(grid[0])):
        col = [grid[i][j] for i in range(len(grid))]
        for i, sc in enumerate(line_scenic_scores(col)):
            scenic_map[i][j] *= sc

    return scenic_map


def max_scenic_score(grid: list[list[int]]) -> int:
    """Compute max scenic score from tree heights

    Parameters
    ----------
    grid : list[list[int]]
        Grid of tree heights

    Returns
    -------
    int
        Max scenic score
    """
    return max(max(row) for row in scenic_scores(grid))


def print_grid(grid: list[list[int]]) -> str:
    """Utility method to print the tree height and scenic score grid

    Parameters
    ----------
    grid : list[list[int]]
        Tree height or scenic score grid

    Returns
    -------
    str
        String representation of grid
    """
    return "\n".join("".join(str(tree) for tree in row) for row in grid)


@click.command()
@click.argument("fn", type=click.Path(exists=True, dir_okay=False))
def main(fn: str):
    """Solution for day 08

    FN should point to the input file for today.

    \f
    Parameters
    ----------
    fn : str
        Path to input file
    """
    in_fn = Path(fn)

    grid: list[list[int]] = []
    with open(in_fn, "r") as f:
        for line in f.readlines():
            grid.append([int(c) for c in line.strip()])

    visible = visibility_map(grid)
    # print(print_visibility_map(visible))
    count = count_visible(visible)
    print(f"Task 01: {count}")

    # scenic_map = scenic_scores(grid)
    # print(print_grid(scenic_map))
    scenic_score = max_scenic_score(grid)
    print(f"Task 02: {scenic_score}")


if __name__ == "__main__":
    main()
