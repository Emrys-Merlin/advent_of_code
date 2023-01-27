import re
from dataclasses import dataclass, field
from heapq import heappop, heappush
from pathlib import Path
from typing import Iterator

import click
from tqdm import tqdm


@dataclass(frozen=True)
class Resources:
    """Collectable resources"""

    ore: int = 0
    clay: int = 0
    obsidian: int = 0
    geode: int = 0

    def __add__(self, other: "Resources") -> "Resources":
        """Add two resource piles

        Parameters
        ----------
        other : Resources
            Other resources

        Returns
        -------
        Resources
            Componentwise sum
        """
        return Resources(
            **{
                resource: getattr(self, resource) + getattr(other, resource)
                for resource in ["ore", "clay", "obsidian", "geode"]
            }
        )

    def __sub__(self, other: "Resources") -> "Resources":
        """Subtract two resource piles

        Parameters
        ----------
        other : Resources
            Other resources

        Returns
        -------
        Resources
            Componentwise difference
        """
        return Resources(
            **{
                resource: getattr(self, resource) - getattr(other, resource)
                for resource in ["ore", "clay", "obsidian", "geode"]
            }
        )


@dataclass(frozen=True)
class State:
    """State of the playing field

    Parameters
    -------
    time_remaining : int
        Remaining time for construction and collection
    *_robots : int
        Number of available collection robots of their kind
    resources : Resource
        Available resources

    """

    time_remaining: int
    ore_robots: int = 1
    clay_robots: int = 0
    obsidian_robots: int = 0
    geode_robots: int = 0
    resources: Resources = field(default_factory=Resources)

    def next_states(self, blueprint: "Blueprint") -> Iterator["State"]:
        """Iterate over possible follow states

        Yields
        ------
        State
            Reachable state from the current state
        """
        max_new_resources = self.resources + Resources(
            ore=self.ore_robots,
            clay=self.clay_robots,
            obsidian=self.obsidian_robots,
            geode=self.geode_robots,
        )
        new_time_remaining = self.time_remaining - 1

        # Build no new robots
        yield State(
            ore_robots=self.ore_robots,
            clay_robots=self.clay_robots,
            obsidian_robots=self.obsidian_robots,
            geode_robots=self.geode_robots,
            resources=max_new_resources,
            time_remaining=new_time_remaining,
        )

        # Temporary save of current robots
        robot_army = {
            "ore_robots": self.ore_robots,
            "clay_robots": self.clay_robots,
            "obsidian_robots": self.obsidian_robots,
            "geode_robots": self.geode_robots,
        }

        # Try to build new robots
        for resource in reversed(["ore", "clay", "obsidian", "geode"]):  #
            req_resources = getattr(blueprint, f"{resource}_robot")
            # Check if we have enough resources to build a robot
            # and if we could have built the robot earlier.
            if self.robot_necessary(req_resources):
                new_resources = max_new_resources - req_resources
                robot_army[f"{resource}_robots"] += 1
                yield State(
                    resources=new_resources,
                    time_remaining=new_time_remaining,
                    **robot_army,
                )
                robot_army[f"{resource}_robots"] -= 1

    def robot_necessary(self, req: Resources) -> bool:
        """Check if the robot can and should be built

        The 'all' statement below handles the 'can' part.
        The 'any' statement the 'should' part to reduce
        the search space. In particular, we only built
        a robot the first iteration we have enough materials
        to do so.

        Parameters
        ----------
        req : Resources
            Resources required for the robot

        Returns
        -------
        bool
            True, if robot can and should be built
        """
        # Any reduces search space by only building a robot
        # the first time we have enough resources to do so.
        # FIXME I was sure that '<' would be sufficient in
        # the 'any' statement, but this implementation failed
        # for task02. Sadly, this simple switch from '<' to
        # '<=' drastically increases the runtime... Still,
        # it produces the correct solution.
        return all(
            getattr(self.resources - req, resource) >= 0
            for resource in ["ore", "clay", "obsidian", "geode"]
        ) and any(
            getattr(self.resources - req, resource)
            <= getattr(self, f"{resource}_robots")
            for resource in ["ore", "clay", "obsidian", "geode"]
            if getattr(req, resource) > 0
        )

    @property
    def max_potential_geodes(self) -> int:
        """Upper bound for collected geodes at end of collection period

        Assumes that in each remaining time step another
        geode robot is built.

        Returns
        -------
        int
            Upper bound
        """
        n = self.time_remaining
        # current geodes + geodes harvested by current amount of robots in remaining time
        # + additionally harvested geodes assuming one geode bot is built in each of the
        # remaining time steps.
        return self.resources.geode + self.geode_robots * n + n * (n - 1) // 2

    def __lt__(self, other: "State") -> bool:
        """Lower than comparison

        Caution: Actually implements greater than, because heapq implements a
        min-heap but we need a max-heap

        Comparison is based on the upper bound of collectable geodes of each state.

        Parameters
        ----------
        other : State
            Other state

        Returns
        -------
        bool
            True, if self > other (based on geode upper bound)
        """
        return self.max_potential_geodes > other.max_potential_geodes


@dataclass()
class Blueprint:
    """Representation of robot blueprints

    Parameters
    -------
    *_robot : Resouces
        Resources needed to construct a robot of the associated type.
    """

    ore_robot: Resources
    clay_robot: Resources
    obsidian_robot: Resources
    geode_robot: Resources

    PATTERN = r"Blueprint \d+: Each ore robot costs (\d+) ore\. Each clay robot costs (\d+) ore\. Each obsidian robot costs (\d+) ore and (\d+) clay\. Each geode robot costs (\d+) ore and (\d+) obsidian\."

    @classmethod
    def from_line(cls, line: str) -> "Blueprint":
        """Parse blueprint from input line

        Parameters
        ----------
        line : str
            A single input line

        Returns
        -------
        Blueprint
            Robot blueprints
        """
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
        """Maximum number of geodes collectable with current blueprint in time limit

        Implements the A* algorithm using the upper bound on collectable geodes for
        each state as the distance estimate.

        Parameters
        ----------
        time_limit : int, optional
            Time limit for construction and collection, by default 24

        Returns
        -------
        int
            Maximum number of geodes
        """
        initial_state = State(time_remaining=time_limit)
        heap = [initial_state]
        visited: set[State] = set(heap)
        max_geodes = 0

        while len(heap) != 0:
            state = heappop(heap)

            if state.time_remaining == 0:
                return state.resources.geode

            for new_state in state.next_states(self):

                if new_state in visited:
                    continue

                visited.add(new_state)

                if new_state.max_potential_geodes < max_geodes:
                    continue

                if new_state.resources.geode > max_geodes:
                    max_geodes = state.resources.geode

                heappush(heap, new_state)

        return -1


class RobotFactory:
    def __init__(self, input: str):
        """Model for robot factory with blueprints

        Parameters
        ----------
        input : str
            Input to parse
        """
        self.blueprints = [
            Blueprint.from_line(line.strip())
            for line in input.split("\n")
            if len(line.strip()) != 0
        ]

    def fingerprint(
        self, time_limit: int = 24, max_blueprints: int | None = None
    ) -> tuple[int, int]:
        """Compute factory fingerprints (aka task solutions)

        Parameters
        ----------
        time_limit : int, optional
            Time limit for collection and construction, by default 24
        max_blueprints : int | None, optional
            Maximum available blueprints (all, if None), by default None

        Returns
        -------
        tuple[int, int]
            total_quality_levels, product of max_geodes
        """
        res = 0
        res2 = 1
        if max_blueprints is None:
            blueprints = self.blueprints
        else:
            blueprints = self.blueprints[:max_blueprints]
        for i, blueprint in tqdm(
            enumerate(blueprints, 1),
            total=len(blueprints),
        ):
            max_geodes = blueprint.max_geodes(time_limit=time_limit)
            res += i * max_geodes
            res2 *= max_geodes

        return res, res2


@click.command()
@click.argument("fn", type=click.Path(exists=True, dir_okay=False))
def main(fn: str):
    """Solution for day 19

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

    robot_factory = RobotFactory(input)

    tqf, _ = robot_factory.fingerprint()
    print(f"Task01: {tqf}")

    _, geode_product = robot_factory.fingerprint(time_limit=32, max_blueprints=3)
    print(f"Task02: {geode_product}")


if __name__ == "__main__":
    main()
