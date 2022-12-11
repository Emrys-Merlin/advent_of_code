from collections import deque
from dataclasses import dataclass, field
from pathlib import Path

import click


@dataclass
class CPU:
    """Runs the program

    Returns
    -------
    CPU
        Runs the program
    """

    x = 1
    summands: deque[int] = field(default_factory=lambda: deque([]), init=False)

    def run_program(self, program: str) -> tuple[int, str]:
        """Run program

        Parameters
        ----------
        program : str
            The puzzle input

        Returns
        -------
        tuple[int, str]
            Returns [signal_strength, ASCII Art of plot]
        """
        program = program.strip()

        signal_strength = 0
        completed = True
        line_iterator = iter(program.split("\n"))
        cycle = 0
        output = ""
        offset = 0
        while True:
            cycle += 1

            if abs(self.x - (cycle - 1 - offset)) <= 1:
                output += "·"
            else:
                output += " "

            if cycle % 40 == 0:
                offset += 40
                output += "\n"

            if (cycle - 20) % 40 == 0:
                current_strength = cycle * self.x
                signal_strength += current_strength

            if completed:
                try:
                    line = next(line_iterator)
                    if line != "noop":
                        completed = False
                        summand = int(line.split(" ")[1])
                except StopIteration:
                    break
            else:
                self.x += summand
                completed = True

        return signal_strength, output


@click.command()
@click.argument("fn", type=click.Path(exists=True, dir_okay=False))
def main(fn: str):
    """Solution for day 10

    FN should point to the input file for today.

    \f
    Parameters
    ----------
    fn : str
        Path to input file
    """
    in_fn = Path(fn)

    with open(in_fn, "r") as f:
        program = f.read()

    cpu = CPU()
    signal_strength, output = cpu.run_program(program)
    print(f"Task 01: {signal_strength}")
    print(f"Task 02:\n{output}")


if __name__ == "__main__":
    main()
