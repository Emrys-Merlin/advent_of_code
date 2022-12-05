import itertools as it
import re
from dataclasses import dataclass
from pathlib import Path
from typing import Optional

import click


@dataclass
class Instruction:
    instr: str
    a: str
    b: Optional[str | int] = None


@dataclass
class ALU:
    instructions: list[Instruction]

    def __call__(
        self,
        input: str,
        variable: str = "z",
        instruction_counter: int = 0,
        variable_init: int = 0,
    ) -> tuple[int, int]:
        variables: dict[str, int] = {}
        variables[variable] = variable_init
        i = 0

        for ic, instruction in enumerate(
            self.instructions[instruction_counter:], instruction_counter
        ):
            if instruction.b in ["w", "x", "y", "z"]:
                tmp = variables.get(instruction.b, 0)  # type: ignore[arg-type]
            elif instruction.b is not None:
                tmp = int(instruction.b)

            if instruction.instr == "inp":
                try:
                    variables[instruction.a] = int(input[i])
                    i += 1
                except IndexError:
                    break

            elif instruction.instr == "add":
                variables[instruction.a] = variables.get(instruction.a, 0) + tmp

            elif instruction.instr == "mul":
                variables[instruction.a] = variables.get(instruction.a, 0) * tmp

            elif instruction.instr == "div":
                variables[instruction.a] = variables.get(instruction.a, 0) // tmp

            elif instruction.instr == "mod":
                variables[instruction.a] = variables.get(instruction.a, 0) % tmp

            elif instruction.instr == "eql":
                variables[instruction.a] = int(variables.get(instruction.a, 0) == tmp)

            else:
                raise Exception(f"Cannot interpret instruction {instruction.instr=}.")

        return variables.get(variable, 0), ic


# Solution from
# https://www.keiruaprod.fr/blog/2021/12/29/a-comprehensive-guide-to-aoc-2021-day-24.html
# I was stuck and used the above solution to understand the approach.
# This is not my solution.
def extract_parameters(program: str) -> list[list[int]]:
    """Extract variable parameters from each block in the ALU program

    Parameters
    ----------
    program : str
        String representation of the ALU program

    Returns
    -------
    list[list[int]]
        List of variable parameters. Each entry is a list with 3 elements: the divisor, the equal check offset, and the summand.

    """
    repeated_program = r"""inp w
mul x 0
add x z
mod x 26
div z (.*)
add x (.*)
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y (.*)
mul y x
add z y"""

    div_check_add = re.findall(repeated_program, program)
    assert len(div_check_add) == 14, len(div_check_add)
    return [list(map(int, dca)) for dca in div_check_add]


def backward(xcheck: int, yadd: int, zdiv: int, z_final: int, w: int) -> list[int]:
    """Returns the possible values of z before a single block
    if the final value of z is z_final and input is w

    Parameters
    ----------
    xcheck : int
        The offset at the x equal check
    yadd : int
        Number added to y
    zdiv : int
        Divisor of z
    z_final : int
        The z score at the end of the (forward) block
    w : int
        The input digit to use


    Returns
    -------
    list[int]
        Possible z values before the block
    """
    zs = []
    x = z_final - w - yadd
    if x % 26 == 0:
        zs.append(x // 26 * zdiv)
    if 0 <= w - xcheck < 26:
        z0 = z_final * zdiv
        zs.append(w - xcheck + z0)

    return zs


def solve(div_check_add: list[list[int]], part: int = 1) -> str:
    """Goes backward through each block and finds possible digits w

    Parameters
    ----------
    div_check_add : list[list[int]]
        List of the three variable parameters in each block
    part : int, optional
        Indicate if task 01 or task 02 is solved, by default 1

    Returns
    -------
    str
        Valid serial number as a string
    """
    zs = {0}
    result: dict[int, tuple[int, ...]] = {}
    if part == 1:
        ws = list(range(1, 10))
    else:
        ws = list(range(9, 0, -1))
    for zdiv, xcheck, yadd in div_check_add[::-1]:
        newzs = set()
        for w, z in it.product(ws, zs):
            z0s = backward(xcheck, yadd, zdiv, z, w)
            for z0 in z0s:
                newzs.add(z0)
                result[z0] = (w,) + result.get(z, ())
        zs = newzs
    return "".join(map(str, result[0]))


@click.command()
@click.argument("fn", type=click.Path(exists=True, dir_okay=False))
def main(fn: str):
    """Solution for day 24

    FN should point to the input file.

    \f
    Parameters
    ----------
    fn : str
        Path to input file
    """
    in_fn = Path(fn)

    instructions = []
    with open(in_fn, "r") as f:
        for line in f.readlines():
            instr = Instruction(*line.strip().split(" "))
            instructions.append(instr)
        f.seek(0)
        div_check_add = extract_parameters(f.read())

    sn = solve(div_check_add=div_check_add, part=1)
    alu = ALU(instructions)
    z, _ = alu(sn)
    print(f"Task 01: {sn}\t{z=}")

    sn = solve(div_check_add=div_check_add, part=2)
    z, _ = alu(sn)
    print(f"Task 02: {sn}\t{z=}")


if __name__ == "__main__":
    main()
