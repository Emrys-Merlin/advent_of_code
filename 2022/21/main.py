import re
from copy import deepcopy
from dataclasses import dataclass
from pathlib import Path
from typing import Union

import click


@dataclass(frozen=True)
class Unit:
    """Arithmetic for degree 1 polynomial

    Parameters
    ----------
    b : float
        bias or constant coefficient of the polynomial
    x : float
        leading coefficient
    """

    b: float
    x: float = 0.0

    def __add__(self, other: Union[int, float, "Unit"]) -> "Unit":
        if isinstance(other, Unit):
            return Unit(b=self.b + other.b, x=self.x + other.x)
        elif isinstance(other, int | float):
            return Unit(b=self.b + other, x=self.x)
        else:
            return NotImplemented

    def __sub__(self, other: Union[int, float, "Unit"]) -> "Unit":
        if isinstance(other, Unit):
            return Unit(b=self.b - other.b, x=self.x - other.x)
        elif isinstance(other, int | float):
            return Unit(b=self.b - other, x=self.x)
        else:
            return NotImplemented

    def __mul__(self, other: int | float) -> "Unit":
        return Unit(b=self.b * other, x=self.x * other)

    def __floordiv__(self, other: int) -> "Unit":
        return Unit(b=self.b // other, x=self.x // other)

    def __truediv__(self, other: int | float) -> "Unit":
        return Unit(b=self.b / other, x=self.x / other)

    def __str__(self) -> str:
        return f"{self.x}x + {self.b}"

    def __rmul__(self, other: int | float) -> "Unit":
        return self.__mul__(other)

    def __radd__(self, other: Union[int, float, "Unit"]) -> "Unit":
        return self.__add__(other)

    def __rsub__(self, other: int | float) -> "Unit":
        return Unit(b=other - self.b, x=-self.x)


@dataclass
class Operator:
    """Operator monkey

    Parameters
    -------
    left : str | int | Unit | float
        Left operand
    right : str | int | Unit | float
        right operand
    operator : str
        Symbol representing binary operator (has to be +, -, *, /)
    """

    left: str | int | Unit | float
    right: str | int | Unit | float
    operator: str
    OPERATIONS = {
        "+": lambda x, y: x + y,
        "-": lambda x, y: x - y,
        "*": lambda x, y: x * y,
        "/": lambda x, y: x / y,
    }

    def ready(self) -> bool:
        """Checks if the operation can be performed

        Returns
        -------
        bool
            True, if both operands are nombers or a polynomial
        """
        return isinstance(self.left, (int, Unit, float)) and isinstance(
            self.right, (int, Unit, float)
        )

    def __call__(self) -> float | Unit:
        """Perform operation

        Returns
        -------
        float | Unit
            Run binary operator on numbers or polynomials
        """
        return self.OPERATIONS[self.operator](self.left, self.right)

    def __str__(self) -> str:
        return f"{self.left} {self.operator} {self.right}"


class Monkeys:
    PATTERN = r"([a-z]{4}): ([a-z]{4}) ([\+\-\*\/]) ([a-z]{4})"

    def __init__(self, input: str) -> None:
        """Initialize monkeys

        Parameters
        ----------
        input : str
            String input
        """
        # Monkeys that shout numbers
        self.number_monkeys: dict[str, int] = {}
        # Monkeys that represent operations
        self.operator_monkeys: dict[str, Operator] = {}

        for line in input.split("\n"):
            line = line.strip()
            if len(line) == 0:
                continue

            match = re.match(self.PATTERN, line)
            if match is None:
                name, number = line.split(": ")
                self.number_monkeys[name] = int(number)
                continue

            self.operator_monkeys[match.group(1)] = Operator(
                left=match.group(2),
                right=match.group(4),
                operator=match.group(3),
            )

    def compute(self, solve_equation: bool = False) -> int:
        """Compute the task results

        The idea is to go through the list of operator monkeys,
        seeing of both operands are available, if yes compute the
        results, remove the operator monkey from the list and repeat
        until root can be computed.

        For the second task (after manually checking, that the system
        of equations is linear in humn), we do the same thing, but
        replacing humn with the polynomial 'x' and replacing the operator
        in root with '-'. Then the result in root is a polynomial a*x + b
        and because of the difference, the equality that the root monkey is
        supposed to check translates to a*x + b = 0, which can be rearranged
        to x = -b/a (if a != 0), which is the solution to the second task.

        Because the coefficients to the systems of equations can become rational,
        we need to work with float :-( So there are rounding errors, but they are
        acceptable...

        Parameters
        ----------
        solve_equation : bool, optional
            if True, run Task02, else Task01, by default False

        Returns
        -------
        int
            Either solution to Task01 or Task01
        """
        operators = deepcopy(self.operator_monkeys)
        remaining_operators: dict[str, Operator] = {}
        number_monkeys: dict[str, int | float | Unit] = dict(**self.number_monkeys)

        if solve_equation:
            # For Task02, we need root to be subtraction
            # and humn to be the variable (or polynomial) x
            op = operators["root"]
            operators["root"] = Operator(left=op.left, right=op.right, operator="-")
            number_monkeys["humn"] = Unit(b=0, x=1)

        # Iteratively try to perform operations until root contains a number
        # or a polynomial
        while "root" not in number_monkeys:
            for name, operator in operators.items():
                if isinstance(operator.left, str) and operator.left in number_monkeys:
                    operator.left = number_monkeys[operator.left]
                if isinstance(operator.right, str) and operator.right in number_monkeys:
                    operator.right = number_monkeys[operator.right]

                if operator.ready():
                    number_monkeys[name] = operator()
                    continue

                remaining_operators[name] = operator

            operators = remaining_operators
            remaining_operators = {}

        root = number_monkeys["root"]
        # For Task01, we just report root
        if not solve_equation:
            assert isinstance(root, float)
            assert int(root) == root
            return int(root)

        # Otherwise, we have to solve for x
        # and deal with rounding errors.
        assert isinstance(root, Unit)
        assert root.x != 0
        res = -root.b / root.x
        # Necessary, because of float errors...
        assert abs(round(res) - res) < 1e-2
        return round(res)

    def inspect_reduced_operators(self) -> dict[str, Operator]:
        """Substitution reduction of system of equations

        Utility function to check if the system of equations
        is linear in humn. It was not obvious to me from the start
        because of all the multiplication and division which might
        make things difficutl.

        Returns
        -------
        dict[str, Operator]
            Reduced set of equations
        """
        operators = {name: op for name, op in self.operator_monkeys.items()}
        operators["root"].operator = "-"
        numbers: dict[str, float | Unit] = {
            name: number
            for name, number in self.number_monkeys.items()
            if name != "humn"
        }

        remaining_operators: dict[str, Operator] = {}

        stop = False
        while True:
            for name, operator in operators.items():
                if isinstance(operator.left, str) and operator.left in numbers:
                    operator.left = numbers[operator.left]
                if isinstance(operator.right, str) and operator.right in numbers:
                    operator.right = numbers[operator.right]

                if operator.ready():
                    numbers[name] = operator()
                    continue

                remaining_operators[name] = operator

            if len(operators) == len(remaining_operators):
                if not stop:
                    stop = True
                else:
                    break

            operators = remaining_operators
            remaining_operators = {}

        return remaining_operators


@click.command()
@click.argument("fn", type=click.Path(exists=True, dir_okay=False))
def main(fn: str):
    """Solution for day 21

    FN should point to the input file for today.

    \f
    Parameters
    ----------
    fn : str
        Path to input file
    """
    in_fn = Path(fn)

    with open(in_fn, "r") as f:
        input = f.read()

    monkeys = Monkeys(input)
    res = monkeys.compute()
    print(f"Task01: {res}")

    print("\nManually inspect that the system of equations is linear in humn")
    reduced_operators = monkeys.inspect_reduced_operators()
    for name, operator in reduced_operators.items():
        print(f"{name} = {operator}")

    monkeys.operator_monkeys = reduced_operators
    res = monkeys.compute(solve_equation=True)
    print(f"\nTask02: {res}")


if __name__ == "__main__":
    main()
