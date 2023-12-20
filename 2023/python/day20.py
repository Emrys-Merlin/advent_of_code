from abc import ABC, abstractmethod
from collections import deque
from dataclasses import dataclass, field
from math import lcm
from pathlib import Path
from typing import Iterable, Iterator

from loguru import logger
from typer import Typer
from utils import timer

main = Typer()


@dataclass(frozen=True)
class Pulse:
    """Encode pulse with source, destination and state (True/False)"""

    destination: str
    state: bool
    source: str = "button"


@dataclass
class Module(ABC):
    """Module base class

    Attributes:
        name: Module name to put in pulse
        neighbors: List of following module names
    """

    name: str
    neighbors: list[str]

    @abstractmethod
    def process_pulse(self, pulse: Pulse) -> Iterator[Pulse]:
        """Process input pulse and yield pulses to neighbors

        Args:
            pulse: Input pulse

        Yields:
            Pulses to all neighbors
        """
        ...


@dataclass
class FlipFlop(Module):
    """FlipFlop module"""

    state = False

    def process_pulse(self, pulse: Pulse) -> Iterator[Pulse]:
        if pulse.state:
            return

        self.state = not self.state
        for neighbor in self.neighbors:
            yield Pulse(
                source=self.name,
                destination=neighbor,
                state=self.state,
            )


@dataclass
class Conjunction(Module):
    """Conjunction module"""

    input_neighbors: dict[str, bool] = field(default_factory=dict)

    def process_pulse(self, pulse: Pulse) -> Iterator[Pulse]:
        self.input_neighbors[pulse.source] = pulse.state

        if all(self.input_neighbors.values()):
            state = False
        else:
            state = True

        for neighbor in self.neighbors:
            yield Pulse(
                source=self.name,
                destination=neighbor,
                state=state,
            )


@dataclass
class Broadcaster(Module):
    """Broadcaster module"""

    def process_pulse(self, pulse: Pulse) -> Iterator[Pulse]:
        for neighbor in self.neighbors:
            yield Pulse(
                source=self.name,
                destination=neighbor,
                state=pulse.state,
            )


@dataclass
class Network:
    """Network/graph abstaction

    Attributes:
        modules: Dict of modules with name as key

    """

    modules: dict[str, Module]

    @classmethod
    def from_input(cls, input: str) -> "Network":
        """Read modules from input string"""
        modules: dict[str, Module] = {}
        conjunction_inputs: dict[str, dict[str, bool]] = {}
        for line in input.splitlines():
            line = line.strip()

            sender, receivers = line.split(" -> ")

            if sender == "broadcaster":
                module = Broadcaster(
                    name=sender,
                    neighbors=receivers.strip().split(", "),
                )
            elif sender.startswith("%"):
                sender = sender[1:]
                module = FlipFlop(
                    name=sender,
                    neighbors=receivers.strip().split(", "),
                )
            elif sender.startswith("&"):
                sender = sender[1:]
                module = Conjunction(
                    name=sender,
                    neighbors=receivers.strip().split(", "),
                )
                conjunction_inputs[sender] = {}
            else:
                raise ValueError(f"Unknown module type: {sender}")

            modules[sender] = module

        for sender, module in modules.items():
            for conjunction in set(module.neighbors).intersection(
                conjunction_inputs.keys()
            ):
                conjunction_inputs[conjunction][sender] = False

        for conjunction, inputs in conjunction_inputs.items():
            modules[conjunction].input_neighbors = inputs

        return cls(modules)

    def count_pulses(self, n_start_pulses: int = 1_000) -> int:
        """Count hi and lo pulses over n_start_pulses

        Args:
            n_start_pulses: Number of button pushes. Defaults to 1_000.

        Returns:
            n_hi_pulses * n_lo_pulses
        """
        lo_count = 0
        hi_count = 0

        for _ in range(n_start_pulses):
            queue = deque([Pulse(destination="broadcaster", state=False)])
            while queue:
                pulse = queue.popleft()
                if pulse.state:
                    hi_count += 1
                else:
                    lo_count += 1

                if pulse.destination not in self.modules:
                    continue

                module = self.modules[pulse.destination]
                for pulse in module.process_pulse(pulse):
                    queue.append(pulse)

        return lo_count * hi_count

    def min_count(self, destination: str = "rx", state: bool = False) -> int:
        """Brute force implementation to find minimum number of button pushes

        Way too inefficient to be used in task02.

        Args:
            destination: Target module. Defaults to "rx".
            state: Target pulse. Defaults to False.

        Returns:
            Minimum number of button pushes
        """
        counter = 0

        while True:
            queue = deque([Pulse(destination="broadcaster", state=False)])
            counter += 1
            received_pulses = 0
            while queue:
                pulse = queue.popleft()

                if pulse.destination == destination and pulse.state == state:
                    received_pulses += 1

                if pulse.destination not in self.modules:
                    continue

                module = self.modules[pulse.destination]
                for pulse in module.process_pulse(pulse):
                    queue.append(pulse)

            if received_pulses == 1:
                return counter

    def find_cycles(
        self,
        source_modules: Iterable[str],
        destination_module: str = "ql",
        state: bool = True,
    ) -> dict[str, tuple[int, int]]:
        """Heuristic to find cycles in network

        In the data we see:
        fh, mf, fz, ss -> &ql -> rx
        ql is an inverter, so for rx to receive lo
        ql needs to receive hi from all its four inputs.
        Hence, we try to figure out when all for input nodes
        are hi at the same time.

        Simplifying assumption: Two consecutive hi pulses in a node
        already indicate a cycle. That might not be true, but turns out
        to be in this case. Also it turns out that the offsets == cycle length
        for all for nodes. Which is exploited in the task02 solution.

        Args:
            source_modules: modules connected to destination module
            destination_module: destination module. Defaults to "ql".
            state: Pulse state to destination module. Defaults to True.

        Returns:
            dict of module name of source_modules to cycle length and offset
        """
        counter = 0
        module_set = set(source_modules)
        cycles_offsets: dict[str, tuple[int, int]] = {}

        while True:
            # for _ in range(2):
            queue = deque([Pulse(destination="broadcaster", state=False)])
            counter += 1
            while queue:
                pulse = queue.popleft()

                if pulse.destination == destination_module and pulse.state == state:
                    if pulse.source in cycles_offsets:
                        cycles_offsets[pulse.source] = (
                            counter - cycles_offsets[pulse.source][1],
                            cycles_offsets[pulse.source][1],
                        )
                        module_set.remove(pulse.source)
                    else:
                        cycles_offsets[pulse.source] = (0, counter)

                if pulse.destination not in self.modules:
                    continue

                module = self.modules[pulse.destination]
                for pulse in module.process_pulse(pulse):
                    queue.append(pulse)

            if len(module_set) == 0:
                return cycles_offsets


@timer
def task01(input: str) -> int:
    """Solution for task 01

    Args:
        input: Input string

    Returns:
        Number of hi pulses * number of lo pulses for 1_000 button pushes
    """
    network = Network.from_input(input)
    return network.count_pulses()


@timer
def task02(input: str) -> int:
    """Heuristic solution for task 02

    We use a cycle detection heuristic and
    as it turns out the cycle length is the same
    as offset for all four source modules. Hence,
    the chinse remainder theorem simplifies to
    computing the least common multiple. In general,
    this will not hold.

    Args:
        input: Input string

    Returns:
        Minimum number of button pushes for rx to receive lo
    """
    network = Network.from_input(input)
    cycles_offsets = network.find_cycles(source_modules=["fh", "mf", "fz", "ss"])
    # Check simplifying assumption
    assert all(cycle == offset for cycle, offset in cycles_offsets.values())
    return lcm(*[cycle for cycle, _ in cycles_offsets.values()])


@main.command()
def entrypoint(path: Path):
    """CLI entrypoint"""
    with open(path, "r") as f:
        input = f.read().strip()

    logger.info(f"Task01: {task01(input)}")
    logger.info(f"Task02: {task02(input)}")


if __name__ == "__main__":
    main()
