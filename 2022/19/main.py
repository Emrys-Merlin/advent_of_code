import enum
import re
from dataclasses import dataclass, field
from functools import total_ordering
from heapq import heappop, heappush
from pathlib import Path
from typing import Iterator

import click


@total_ordering
@dataclass(frozen=True)
class Resources:
    ore: int = 0
    clay: int = 0
    obsidian: int = 0
    geode: int = 0

    def __add__(self, other: "Resources") -> "Resources":
        return Resources(
            **{
                resource: getattr(self, resource) + getattr(other, resource)
                for resource in ["ore", "clay", "obsidian", "geode"]
            }
        )

    def __sub__(self, other: "Resources") -> "Resources":
        return Resources(
            **{
                resource: getattr(self, resource) - getattr(other, resource)
                for resource in ["ore", "clay", "obsidian", "geode"]
            }
        )

    def __lt__(self, other: "Resources") -> bool:
        return all(
            getattr(self - other, resource) < 0
            for resource in ["ore", "clay", "obsidian", "geode"]
        )

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, Resources):
            return NotImplemented
        return all(
            getattr(self - other, resource) == 0
            for resource in ["ore", "clay", "obsidian", "geode"]
        )


@dataclass(frozen=True)
class State:
    ore_robots: int = 1
    clay_robots: int = 0
    obsidian_robots: int = 0
    geode_robots: int = 0
    resources: Resources = field(default_factory=Resources)
    time: int = 0

    def next_states(self, blueprint: "Blueprint") -> Iterator["State"]:
        max_new_resources = self.resources + Resources(
            ore=self.ore_robots,
            clay=self.clay_robots,
            obsidian=self.obsidian_robots,
            geode=self.geode_robots,
        )
        new_time = self.time + 1

        # Build no new robots
        yield State(
            ore_robots=self.ore_robots,
            clay_robots=self.clay_robots,
            obsidian_robots=self.obsidian_robots,
            geode_robots=self.geode_robots,
            resources=max_new_resources,
            time=new_time,
        )

        robot_army = {
            "ore_robots": self.ore_robots,
            "clay_robots": self.clay_robots,
            "obsidian_robots": self.obsidian_robots,
            "geode_robots": self.geode_robots,
        }
        for resource in ["ore", "clay", "obsidian", "geode"]:
            req_resources = getattr(blueprint, f"{resource}_robot")
            if req_resources <= self.resources:
                new_resources = max_new_resources - req_resources
                robot_army[f"{resource}_robots"] += 1
                yield State(resources=new_resources, time=new_time, **robot_army)
                robot_army[f"{resource}_robots"] -= 1

    def __lt__(self, other: "State") -> bool:
        if self.time < other.time:
            return True

        if self.time > other.time:
            return False

        for resource in ["geode", "obsidian", "clay", "ore"]:
            n_self = getattr(self.resources, resource)
            n_other = getattr(other.resources, resource)
            if n_self > n_other:
                return True

            if n_self < n_other:
                return False

            n_self = getattr(self, f"{resource}_robots")
            n_other = getattr(other, f"{resource}_robots")
            if n_self > n_other:
                return True

            if n_self < n_other:
                return False

        return False


@dataclass()
class Blueprint:
    ore_robot: Resources
    clay_robot: Resources
    obsidian_robot: Resources
    geode_robot: Resources

    PATTERN = r"Blueprint \d+: Each ore robot costs (\d+) ore\. Each clay robot costs (\d+) ore\. Each obsidian robot costs (\d+) ore and (\d+) clay\. Each geode robot costs (\d+) ore and (\d+) obsidian\."

    @classmethod
    def from_line(cls, line: str) -> "Blueprint":
        match = re.match(cls.PATTERN, line)
        assert match is not None

        ore_robot = Resources(ore=int(match.group(1)))
        clay_robot = Resources(ore=int(match.group(2)))
        obsidian_robot = Resources(ore=int(match.group(3)), clay=int(match.group(4)))
        geode_robot = Resources(ore=int(match.group(5)), obsidian=int(match.group(6)))

        return Blueprint(
            ore_robot=ore_robot,
            clay_robot=clay_robot,
            obsidian_robot=obsidian_robot,
            geode_robot=geode_robot,
        )

    def max_geodes(self, time_limit: int = 24) -> int:

        initial_state = State()
        heap = [initial_state]
        visited: set[State] = set(heap)

        while len(heap) != 0:
            state = heappop(heap)
            print(state)

            if state.time == time_limit:
                return state.resources.geode

            # print("Next states:")
            for new_state in state.next_states(self):
                # print(new_state)

                if new_state in visited:
                    continue

                visited.add(new_state)
                heappush(heap, new_state)

            # print("####")

        return -1


class RobotFactory:
    def __init__(self, input: str):
        self.blueprints = [
            Blueprint.from_line(line.strip()) for line in input.split("\n")
        ]

    def total_quality_level(self) -> int:
        res = 0
        for i, blueprint in enumerate(self.blueprints, 1):
            print(blueprint)
            max_geodes = blueprint.max_geodes()
            res += i * max_geodes

        return res


@click.command()
@click.argument("fn", type=click.Path(exists=True, dir_okay=False))
def main(fn: str):
    in_fn = Path(fn)

    with open(in_fn, "r") as f:
        input = f.read()

    robot_factory = RobotFactory(input)

    tqf = robot_factory.total_quality_level()
    print(f"Task01: {tqf}")


if __name__ == "__main__":
    main()
