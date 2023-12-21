from collections import defaultdict, deque
from dataclasses import dataclass
from pathlib import Path
from typing import Iterator

from loguru import logger
from typer import Typer
from utils import timer

main = Typer()


@dataclass
class Grid:
    grid: list[list[str]]
    start: tuple[int, int]

    @classmethod
    def from_input(cls, input: str) -> "Grid":
        grid: list[list[str]] = []
        start = (-1, -1)
        for row, line in enumerate(input.splitlines()):
            grid.append(list(line))
            col = line.find("S")
            if col != -1:
                start = (row, col)

        if start[0] == -1 or start[1] == -1:
            raise ValueError("No start found")

        return cls(grid, start)

    def __getitem__(self, pos: tuple[int, int]) -> str:
        row = pos[0]
        if row < 0:
            n = abs(row) // self.height + 1
            row += n * self.height
        row %= self.height
        col = pos[1]
        if col < 0:
            n = abs(col) // self.width + 1
            col += n * self.width
        col %= self.width
        return self.grid[row][col]

    @property
    def width(self) -> int:
        return len(self.grid[0])

    @property
    def height(self) -> int:
        return len(self.grid)

    @property
    def n_patches(self) -> int:
        return sum(char != "#" for line in self.grid for char in line)

    def __str__(self) -> str:
        return "\n".join("".join(row) for row in self.grid)

    def neighbors(
        self, pos: tuple[int, int], infinite: bool = False
    ) -> Iterator[tuple[int, int]]:
        row, col = pos
        for delta_row, delta_col in [(0, -1), (0, 1), (-1, 0), (1, 0)]:
            new_row, new_col = row + delta_row, col + delta_col
            if (
                infinite or (0 <= new_col < self.height and 0 <= new_col < self.width)
            ) and self[new_row, new_col] != "#":
                yield (new_row, new_col)

    def neighbors_with_glued_borders(
        self, coord: tuple[int, int], fields: set[tuple[int, int]]
    ) -> Iterator[tuple[tuple[int, int], set[tuple[int, int]]]]:
        row, col = coord
        for delta_row, delta_col in [(0, -1), (0, 1), (-1, 0), (1, 0)]:
            new_row, new_col = row + delta_row, col + delta_col
            f_delta_row = f_delta_col = 0
            if new_row < 0:
                new_row += self.height
                f_delta_row = -1
            elif new_row >= self.height:
                new_row -= self.height
                f_delta_row = 1

            if new_col < 0:
                new_col += self.width
                f_delta_col = -1
            elif new_col >= self.width:
                new_col -= self.width
                f_delta_col = 1

            if self[new_row, new_col] != "#":
                new_fields = {
                    (f_row + f_delta_row, f_col + f_delta_col)
                    for f_row, f_col in fields
                }

                yield ((new_row, new_col), new_fields)

    def walk_patches(self, n_steps: int = 64, infinite: bool = False) -> int:
        queue = deque([(self.start, 0)])
        visited: dict[tuple[int, int], int] = {}

        while queue:
            coord, steps = queue.popleft()

            if coord in visited:
                continue

            visited[coord] = steps

            if steps == n_steps:
                continue

            for neighbor in self.neighbors(coord, infinite=infinite):
                queue.append((neighbor, steps + 1))

        return sum(int((steps % 2) == (n_steps % 2)) for steps in visited.values())

    def walk_with_glued_borders(self, n_steps: int) -> int:
        # patch coord -> map coords
        # last_patches: dict[tuple[int, int], set(tuple[int, int])] = {}
        patches: dict[tuple[int, int], set(tuple[int, int])] = {
            self.start: {(0, 0)},
        }
        # active_coords: list[frozenset[tuple[int, int]]] = []
        # active_coords_lookup: dict[frozenset[tuple[int, int]], tuple[int, int]] = {}
        for step in range(n_steps):
            new_patches: dict[tuple[int, int], set(tuple[int, int])] = defaultdict(set)
            for coord, fields in patches.items():
                for neighbor, new_fields in self.neighbors_with_glued_borders(
                    coord,
                    fields,
                ):
                    new_patches[neighbor].update(new_fields)

            # if set(new_patches.keys()) == set(last_patches.keys()) and (
            #     n_steps % 2
            # ) != (step % 2):
            #     new_n_patches = sum(
            #         len(current_map) for current_map in new_patches.values()
            #     )
            #     last_n_patches = sum(
            #         len(current_map) for current_map in last_patches.values()
            #     )
            #     delta_n_patches = new_n_patches - last_n_patches
            #     # print(f"step: {step}, delta: {delta_n_patches}, new: {new_n_patches}")

            #     n_cycles = (n_steps - (step + 1)) // 2
            #     return new_n_patches + delta_n_patches * n_cycles

            # last_patches = patches
            patches = new_patches

            # actives = frozenset(patches.keys())
            # if actives in active_coords_lookup:
            #     offset, patches_offset = active_coords_lookup[actives]
            #     cycle = step - offset

            #     delta_patches = (
            #         sum(len(current_map) for current_map in patches.values())
            #         - patches_offset
            #     )

            #     n_cycles = (n_steps - offset) // cycle
            #     delta = (n_steps - offset) % cycle
            #     _, patches_delta_offset = active_coords_lookup[active_coords[delta]]

            #     return patches_delta_offset + delta_patches * n_cycles

            # active_coords.append(actives)
            # active_coords_lookup[actives] = (
            #     step,
            #     sum(len(current_map) for current_map in patches.values()),
            # )

        return sum(len(current_map) for current_map in patches.values())

    def visualize(self, reached: set[tuple[int, int]]) -> str:
        res = ""
        for row, line in enumerate(self.grid):
            for col, char in enumerate(line):
                if (row, col) in reached:
                    res += "O"
                else:
                    res += char
            res += "\n"

        return res


@timer
def task01(input: str, n_steps: int = 64) -> int:
    grid = Grid.from_input(input)
    return grid.walk_patches(n_steps=n_steps)


@timer
def task02(input: str, n_steps: int = 26501365) -> int:
    grid = Grid.from_input(input)
    # return grid.walk_patches(n_steps=n_steps, infinite=True)
    return grid.walk_with_glued_borders(n_steps=n_steps)


@main.command()
def entrypoint(path: Path):
    with open(path, "r") as f:
        data = f.read().strip()

    logger.info(f"Task 01: {task01(data)}")
    logger.info(f"Task 02: {task02(data)}")


if __name__ == "__main__":
    main()
