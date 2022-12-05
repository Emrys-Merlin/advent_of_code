import re
from copy import deepcopy
from dataclasses import dataclass
from pathlib import Path

import click


@dataclass
class Instruction:
    """Crane moving instructions"""

    amount: int
    source: int
    dest: int


class Crates:
    def __init__(self, stack: list[str]) -> None:
        """Representation of the crate stacks

        Parameters
        ----------
        stack : list[str]
            The rows of the input file encoding the crate stacks.
        """
        stack = deepcopy(stack)
        self.stacks: list[list[str]] = []
        for _ in stack.pop().strip().split():
            self.stacks.append([])

        while stack:
            row = stack.pop()
            for i in range(len(self.stacks)):
                idx = 1 + 4 * i
                if idx < len(row) and row[idx] != " ":
                    self.stacks[i].append(row[idx])

    def __str__(self) -> str:
        """String representation

        Returns
        -------
        str
            String representation
        """
        width = len(self.stacks)
        rows: list[list[str]] = []
        for col, stack in enumerate(self.stacks):
            for row, crate in enumerate(stack):
                if row >= len(rows):
                    rows.append([" "] * width)

                rows[row][col] = crate

        out = ""
        for rw in rows:
            out = "".join(rw) + "\n" + out

        return out

    def message(self) -> str:
        """Print the solution message

        Returns
        -------
        str
            Solution message
        """
        return "".join(stack[-1] for stack in self.stacks)

    def apply_instruction(self, instruction: Instruction, crane: int = 9000):
        """Apply crane instructions to the crate stack.

        Can handle CrateMover 9000 and CrateMover 9001

        Parameters
        ----------
        instruction : Instruction
            A single set of instructions
        crane : int, optional
            Crane ID, must be 9000 or 90001, by default 9000

        Raises
        ------
        Exception
            If unkown crane ID is passed.
        """
        if crane == 9000:
            stack = self.stacks[instruction.source]
        elif crane == 9001:
            stack = [
                self.stacks[instruction.source].pop() for _ in range(instruction.amount)
            ]
        else:
            raise Exception("Unkown crane.")

        for _ in range(instruction.amount):
            self.stacks[instruction.dest].append(stack.pop())

    def apply_instructions(self, instructions: list[Instruction], crane: int = 9000):
        """Apply a list of crane instructions to the crate stack.

        Can handle CrateMover 9000 and CrateMover 9001

        Parameters
        ----------
        instructions : list[Instruction]
            A list of instructions
        crane : int, optional
            Crane ID, must be 9000 or 9001, by default 9000
        """
        for instruction in instructions:
            self.apply_instruction(instruction, crane)


@click.command()
@click.argument("fn", type=click.Path(exists=True, dir_okay=False))
def main(fn: str):
    """Solution for day 05

    FN should point to the input file for today.

    \f
    Parameters
    ----------
    fn : str
        Path to input file
    """
    in_fn = Path(fn)

    stack = []
    instructions = []
    read_crates = True
    with open(in_fn, "r") as f:
        for line in f.readlines():
            if len(line.strip()) == 0:
                read_crates = False
                continue

            if read_crates:
                stack.append(line)

            else:
                match = re.match(r"move (\d*) from (\d*) to (\d*)", line.strip())
                if match is None:
                    raise Exception("RegEx failed.")

                instructions.append(
                    Instruction(
                        amount=int(match.group(1)),
                        source=int(match.group(2)) - 1,  # 0-based indexing
                        dest=int(match.group(3)) - 1,  # 0-based indexing
                    )
                )

    crates = Crates(stack)
    crates.apply_instructions(instructions)
    print(f"Task 01: {crates.message()}")

    crates = Crates(stack)
    crates.apply_instructions(instructions=instructions, crane=9001)
    print(f"Task 02: {crates.message()}")


if __name__ == "__main__":
    main()
