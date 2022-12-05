from pathlib import Path

import click


class Grid:
    def __init__(self, grid: str) -> None:
        """Create grid representation

        Parameters
        ----------
        grid : str
            String representation read from file
        """
        self.easts = set()
        self.souths = set()
        lines = [
            line.strip() for line in grid.split("\n") if len(line.strip()) != 0
        ]  # fix newline at file end
        self.n_rows = len(lines)
        self.n_cols = len(lines[0])

        for r, line in enumerate(lines):
            for c, field in enumerate(line):
                if field == ">":
                    self.easts.add((r, c))
                elif field == "v":
                    self.souths.add((r, c))

    def __str__(self) -> str:
        """Create string representation of grid

        Returns
        -------
        str
            String representation
        """
        grid = [["."] * self.n_cols for _ in range(self.n_rows)]

        for species, symbol in zip([self.easts, self.souths], [">", "v"]):
            for r, c in species:
                grid[r][c] = symbol

        return "".join("".join(line) + "\n" for line in grid)

    def __call__(self) -> bool:
        """Peform a single sea cucumbers step

        Returns
        -------
        bool
            True if no cucumber moved, i.e. if patterin is stable
        """
        stable = True

        new_easts = set()
        for r, c in self.easts:
            new_pos = (r, (c + 1) % self.n_cols)
            if new_pos in self.easts or new_pos in self.souths:
                new_easts.add((r, c))
            else:
                new_easts.add(new_pos)
                stable = False

        self.easts = new_easts

        new_souths = set()
        for r, c in self.souths:
            new_pos = ((r + 1) % self.n_rows, c)
            if new_pos in self.easts or new_pos in self.souths:
                new_souths.add((r, c))
            else:
                new_souths.add(new_pos)
                stable = False

        self.souths = new_souths

        return stable

    def find_stable(self) -> int:
        """Find stable cucumber configuration

        Returns
        -------
        int
            Number of iterations until convergence
        """
        i = 1

        while not self():
            i += 1

        return i

    def shape(self) -> tuple[int, int]:
        """Shape of the grid

        Returns
        -------
        tuple[int, int]
            (n_rows, n_cols)
        """
        return self.n_rows, self.n_cols


@click.command()
@click.argument("fn", type=click.Path(exists=True, dir_okay=False))
def main(fn: str):
    """Solution for day 25

    FN should point to the input file.

    \f
    Parameters
    ----------
    fn : str
        Path to input file
    """
    in_fn = Path(fn)

    with open(in_fn, "r") as f:
        grid = Grid(f.read())

    print(grid)
    print(grid.shape())

    # grid()
    # print(grid)

    n_stable = grid.find_stable()

    print(grid)

    print(f"Task 01: {n_stable}")


if __name__ == "__main__":
    main()
