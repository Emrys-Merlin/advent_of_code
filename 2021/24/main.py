from collections import namedtuple
from dataclasses import dataclass
from pathlib import Path
from queue import PriorityQueue

import click
from tqdm import tqdm

Instruction = namedtuple("Instruction", ["instr", "a", "b"], defaults=[None] * 3)


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
                tmp = variables.get(instruction.b, 0)
            elif instruction.b is not None:
                tmp = int(instruction.b)

            if instruction.instr == "inp":
                try:
                    variables[instruction.a] = int(input[i])
                    i += 1
                except:
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

            # print(variables)

        return variables.get(variable, 0), ic

    def find_largest_serial_number(self) -> tuple[int, int]:

        min_sn = int(1e15) - 1
        max_sn = int(1e14)
        pq: PriorityQueue[tuple[int, str, int]] = PriorityQueue()
        pq.put((0, "", 0))
        # j = 0
        while pq:
            z, sn, ic = pq.get()
            # print(f"{sn=}\t{z=}\t{ic=}")
            # j += 1
            # if j > 10:
            #     break

            if z > 26 ** (14 - len(sn)):
                # print(f"Discard: {sn=}\t{z=}")
                continue

            # print(f"{sn=}\t{z=}")

            if z == 0 and len(sn) == 14:
                if int(sn) < min_sn:
                    min_sn = int(sn)
                if int(sn) > max_sn:
                    max_sn = int(sn)
                # print(f"{sn=}")
                continue

            for i in range(9, 0, -1):
                cand = sn + str(i)
                new_z, new_ic = self(str(i), variable_init=z, instruction_counter=ic)
                pq.put((new_z, cand, new_ic))

        print(min_sn)
        print(max_sn)

        return min_sn, max_sn


@click.command()
@click.argument("fn", type=click.Path(exists=True, dir_okay=False))
def main(fn: str):
    in_fn = Path(fn)

    instructions = []
    with open(in_fn, "r") as f:
        for line in f.readlines():
            instr = Instruction(*line.strip().split(" "))
            instructions.append(instr)

    alu = ALU(instructions)

    serial_number = alu.find_largest_serial_number()

    print(f"Task 01: {serial_number}")


if __name__ == "__main__":
    main()
