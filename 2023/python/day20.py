from abc import ABC, abstractmethod
from collections import deque
from dataclasses import dataclass, field
from pathlib import Path
from typing import Iterator

from loguru import logger
from typer import Typer
from utils import timer

main = Typer()


@dataclass(frozen=True)
class Pulse:
    destination: str
    state: bool
    origin: str = "button"


@dataclass
class Module(ABC):
    name: str
    neighbors: list[str]

    @abstractmethod
    def process_pulse(self, pulse: Pulse) -> Iterator[Pulse]:
        ...


@dataclass
class FlipFlop(Module):
    state = False

    def process_pulse(self, pulse: Pulse) -> Iterator[Pulse]:
        if pulse.state:
            return

        self.state = not self.state
        for neighbor in self.neighbors:
            yield Pulse(
                origin=self.name,
                destination=neighbor,
                state=self.state,
            )


@dataclass
class Conjunction(Module):
    input_neighbors: dict[str, bool] = field(default_factory=dict)

    def process_pulse(self, pulse: Pulse) -> Iterator[Pulse]:
        self.input_neighbors[pulse.origin] = pulse.state

        if all(self.input_neighbors.values()):
            state = False
        else:
            state = True

        for neighbor in self.neighbors:
            yield Pulse(
                origin=self.name,
                destination=neighbor,
                state=state,
            )


@dataclass
class Broadcaster(Module):
    def process_pulse(self, pulse: Pulse) -> Iterator[Pulse]:
        for neighbor in self.neighbors:
            yield Pulse(
                origin=self.name,
                destination=neighbor,
                state=pulse.state,
            )


@dataclass
class Network:
    modules: dict[str, Module]

    @classmethod
    def from_input(cls, input: str) -> "Network":
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


@timer
def task01(input: str) -> int:
    network = Network.from_input(input)
    return network.count_pulses()


@timer
def task02(input: str) -> int:
    network = Network.from_input(input)
    return network.min_count(destination="rx", state=True)


@main.command()
def entrypoint(path: Path):
    with open(path, "r") as f:
        input = f.read().strip()

    logger.info(f"Task01: {task01(input)}")
    logger.info(f"Task02: {task02(input)}")


if __name__ == "__main__":
    main()
