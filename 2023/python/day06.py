from dataclasses import dataclass
from math import ceil, floor, prod, sqrt
from pathlib import Path

from typer import Typer
from utils import timer

main = Typer()


@dataclass(frozen=True)
class Race:
    """Container representing a race via allowed time and record distance"""

    time: int
    record: int

    def distance_per_time(self, time: int) -> int:
        """Distance traveled per time spent pushing the button

            0 <= time <= self.time

        Args:
            time: Button push time

        Returns:
            Traveled distance
        """
        return time * (self.time - time)

    @property
    def n_winning(self) -> int:
        """How often the record can be beat.

        Computed naively by trying out all possible button push times.
        """
        return sum(
            self.distance_per_time(i) > self.record for i in range(self.time + 1)
        )

    @property
    def n_winning_analytical(self) -> int:
        """How often the record can be beat.

        Computed analytically by solving the quadratic equation:

            -x^2 +tx = d
            x^2 - tx + d = 0
            x = (t +- sqrt(t^2 - 4d)) / 2
        """
        t_lo = (self.time - sqrt(self.time**2 - 4 * self.record)) / 2
        t_hi = (self.time + sqrt(self.time**2 - 4 * self.record)) / 2
        if t_lo == ceil(t_lo):
            t_lo += 1
        if t_hi == floor(t_hi):
            t_hi -= 1
        res = int(floor(t_hi)) - int(ceil(t_lo)) + 1
        return res


@timer
def parse_input01(input: str) -> list[Race]:
    """Parse input according to task 01.

    Interpret numbers separated by spaces as separate numbers
    """
    lines = input.splitlines()
    times = lines[0].split(":")[1].strip().split()
    distances = lines[1].split(":")[1].strip().split()

    return [
        Race(time=int(time), record=int(distance))
        for time, distance in zip(times, distances)
    ]


@timer
def parse_input02(input: str) -> list[Race]:
    """Parse input according to task 02.

    Ignore whitespace and interpret numbers as single numbers
    """
    lines = input.splitlines()
    time = int(lines[0].split(":")[1].strip().replace(" ", ""))
    distance = int(lines[1].split(":")[1].strip().replace(" ", ""))

    return [Race(time=time, record=distance)]


@timer
def winning_product(races: list[Race]) -> int:
    """Multiply all the ways you can win the different races"""
    return prod(race.n_winning_analytical for race in races)


@main.command()
def entrypoint(path: Path):
    """CLI entrypoint"""
    with open(path, "r") as f:
        input = f.read().strip()

    races = parse_input01(input)
    print(f"Task 01: {winning_product(races)}")

    races = parse_input02(input)
    print(f"Task 02: {winning_product(races)}")


if __name__ == "__main__":
    main()
