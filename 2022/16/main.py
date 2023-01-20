import re
from copy import deepcopy
from dataclasses import dataclass
from heapq import heappop, heappush
from itertools import product
from pathlib import Path
from typing import Iterator

import click


@dataclass
class Valve:
    """Representation of a valve (probably overkill)

    Parameters
    ----------
    idx : int
        Index of the corresponding valve entry in the minimal distance matrix
    name : str
        Name of the valve (e.g. AA)
    rate : int
        Flow rate of the valve
    neighbors : list[str]
        Names of the neighboring valves
    """

    idx: int
    name: str
    rate: int
    neighbors: list[str]


@dataclass
class State:
    """Representation of valve-tunnel network

    Parameters
    ----------
    position : str
        (Next) position of the agent
    time_remaining : int
        Remaining time to release pressure
    released_pressure : int
        How much pressure was released so far (computed until the end of the time limit)
    closed_rate : int
        How much total flow rate the closed valves provide
    closed_valves : set[str]
        Which valves are still closed (only taking non-zero valves into account)
    position_elephant : str | None = None
        (Next) position of the elephant agent
    arrival : int = 0
        Time until arrival of the agent at the next position (if 0 the position is the current position)
    arrival_elephant : int = 0
        Time until arrival of the elephant agent at the next position (if 0 the position is the current position)
    """

    position: str
    time_remaining: int
    released_pressure: int
    closed_rate: int
    closed_valves: set[str]
    position_elephant: str | None = None
    arrival: int = 0
    arrival_elephant: int = 0

    @property
    def potential(self) -> int:
        return self.released_pressure + self.closed_rate * self.time_remaining

    def __lt__(self, other: "State") -> bool:
        return self.potential > other.potential

    def __hash__(self) -> int:
        return hash(
            (
                frozenset(
                    [
                        (self.position, self.arrival),
                        (self.position_elephant, self.arrival_elephant),
                    ]
                ),
                self.time_remaining,
                self.released_pressure,
                frozenset(self.closed_valves),
            )
        )


@dataclass(frozen=True)
class Network:
    """Representation of valve-tunnel network

    Parameters
    ----------
    closed_rate : str
        The total rate of all valves
    valves : dict[str, Valve]
        Dictionary with valve names as keys and valves as values
    distances : list[list[int]]
        Minimal distance matrix between all valves. The corresponding valve indices for the
        matrix can be found in Valve.idx
    non_zero_valves : set[str]
        Set containing the valvs with non-zero flow rate
    """

    closed_rate: int
    valves: dict[str, Valve]
    distances: list[list[int]]
    non_zero_valves: set[str]

    @staticmethod
    def from_input(input: str) -> "Network":
        """Create tunnel network from input

        Parameters
        ----------
        input : str
            Input read from file

        Returns
        -------
        Network
            Network
        """
        valves: dict[str, Valve] = {}
        non_zero_valves: set[str] = set()

        closed_rate = 0
        for i, line in enumerate(input.split("\n")):
            line = line.strip()
            if len(line) == 0:
                continue

            match = re.match(
                r"Valve (..) has flow rate=(\d+); tunnels? leads? to valves? (.*)", line
            )
            assert match is not None

            name = match.group(1)
            rate = int(match.group(2))
            neighbors = [neighbor.strip() for neighbor in match.group(3).split(",")]

            valves[name] = Valve(i, name, rate, neighbors)
            closed_rate += rate
            if rate > 0:
                non_zero_valves.add(name)

        distances = Network.floyd_shortest_path(valves)

        return Network(
            closed_rate=closed_rate,
            valves=valves,
            distances=distances,
            non_zero_valves=non_zero_valves,
        )

    @staticmethod
    def floyd_shortest_path(
        valves: dict[str, Valve], no_edge: int = int(1e10)
    ) -> list[list[int]]:
        """Compute shortest paths between all valve pairs

        Parameters
        ----------
        valves : dict[str, Valve]
            Dictionary containing the valves with adjacency lists
        no_edge : int, optional
            high weight denoting a missing edge in  the graph, by default int(1e10)

        Returns
        -------
        list[list[int]]
            Minimal distance matrix between all valves
        """
        distances = [
            [no_edge if i != j else 0 for i in range(len(valves))]
            for j in range(len(valves))
        ]

        for valve in valves.values():
            i = valve.idx
            for neighbor in valve.neighbors:
                j = valves[neighbor].idx
                distances[i][j] = 1

        for k in range(len(valves)):
            for i in range(len(distances)):
                for j in range(len(distances)):
                    if distances[i][j] > distances[i][k] + distances[k][j]:
                        distances[i][j] = distances[i][k] + distances[k][j]

        return distances

    def next_states(self, state: State) -> Iterator[State]:
        """Generate next plausible states from the current state

        Reduce search space by only considering valves with non-zero
        flow rate. That's why we need the minimal distance matrix
        between any two valves

        Parameters
        ----------
        state : State
            Current states

        Yields
        ------
        Iterator[State]
            Child states
        """
        i = self.valves[state.position].idx
        dists = self.distances[i]
        for next_pos in state.closed_valves:
            j = self.valves[next_pos].idx
            dist = dists[j]

            if dist + 1 > state.time_remaining:
                continue

            time_remaining = state.time_remaining - dist - 1
            rate = self.valves[next_pos].rate
            released_pressure = state.released_pressure + rate * time_remaining
            new_closed_valves = deepcopy(state.closed_valves)
            new_closed_valves.remove(next_pos)
            closed_rate = state.closed_rate - rate

            yield State(
                position=next_pos,
                time_remaining=time_remaining,
                released_pressure=released_pressure,
                closed_rate=closed_rate,
                closed_valves=new_closed_valves,
            )

    def _next_paths(
        self, position: str, arrival: int, closed_valves: set[str], time_remaining: int
    ) -> list[tuple[str, int]]:
        """Helper method for the elephant case, if one agent is stell en route

        If the agent has arrived, this method provides ids of possible next
        valves with the time until arrival + opening of the valve.
        This method also only considers closed valves with non-zero flow rate.
        If the agent is still en route, the current destination valve + remaining
        time until arrival is returned.

        Parameters
        ----------
        position : str
            Current destination valve
        arrival : int
            Time until arrival at destination (in min)
        closed_valves : set[str]
            Set of still closed valves
        time_remaining : int
            Time remaining

        Returns
        -------
        list[tuple[str, int]]
            (next valve, time till arrival + opening)
        """
        next_paths: list[tuple[str, int]] = []
        if arrival != 0:
            next_paths.append((position, arrival))
        else:
            i = self.valves[position].idx
            dists = self.distances[i]
            for next_pos in closed_valves:
                j = self.valves[next_pos].idx
                dist = dists[j]

                if dist + 1 > time_remaining:
                    continue

                next_paths.append((next_pos, dist + 1))

        return next_paths

    def next_states_with_elephant(self, state: State) -> Iterator[State]:
        """Generate next states (taking elephant into account)

        Parameters
        ----------
        state : State
            Current state of the valves with positions of agents etc

        Yields
        ------
        Iterator[State]
            Child states
        """
        next_paths = self._next_paths(
            state.position, state.arrival, state.closed_valves, state.time_remaining
        )
        assert state.position_elephant is not None
        next_paths_elephant = self._next_paths(
            state.position_elephant,
            state.arrival_elephant,
            state.closed_valves,
            state.time_remaining,
        )

        for (pos, arrival), (pos_e, arrival_e) in product(
            next_paths, next_paths_elephant
        ):
            min_arrival = min(arrival, arrival_e)
            arrival -= min_arrival
            arrival_e -= min_arrival
            time_remaining = state.time_remaining - min_arrival
            released_pressure = state.released_pressure
            closed_rate = state.closed_rate
            closed_valves = deepcopy(state.closed_valves)

            if arrival == 0 and pos in closed_valves:
                rate = self.valves[pos].rate
                released_pressure += rate * time_remaining
                closed_rate -= rate
                closed_valves.remove(pos)

            if arrival_e == 0 and pos_e in closed_valves:
                rate = self.valves[pos_e].rate
                released_pressure += rate * time_remaining
                closed_rate -= rate
                closed_valves.remove(pos_e)

            yield State(
                position=pos,
                time_remaining=time_remaining,
                released_pressure=released_pressure,
                closed_rate=closed_rate,
                closed_valves=closed_valves,
                position_elephant=pos_e,
                arrival=arrival,
                arrival_elephant=arrival_e,
            )

    def max_pressure_release(
        self, start: str = "AA", time_limit: int = 30, with_elephant: bool = False
    ) -> int:
        """Compute maximum releasable pressure

        This implementation uses something like a weird A* algorithm.
        Each state has a potential, which is the pressure released so far (computed untile the
        end of the time limit) + the pressure that could be released until the end of time limit
        if all valves were opened this instance. This potential is used in the heap to get the most
        promising next candidate. Furthermore, if the potential is smaller than the current maximal
        pressure, we can stop the search.
        It's not really A* in my understanding, because there is no clear end state that stops the
        algorithm. Instead, it's a mixture of stopping after the time limit and taking the potential
        and current max pressure into account.

        Caution! My second task solution runs very slow and takes a couple of minutes. If you spot the
        bottleneck, please let me know ;-)

        Parameters
        ----------
        start : str, optional
            Start position of all agents, by default "AA"
        time_limit : int, optional
            Time limit for valve opening, by default 30
        with_elephant : bool, optional
            Whether an elephant is trained and used, by default False

        Returns
        -------
        int
            Maximum releasable pressure
        """

        if with_elephant:
            time_limit -= 4
            state = State(
                position=start,
                time_remaining=time_limit,
                released_pressure=0,
                closed_rate=self.closed_rate,
                closed_valves=deepcopy(self.non_zero_valves),
                position_elephant=start,
            )
            next_states = self.next_states_with_elephant
        else:
            state = State(
                position=start,
                time_remaining=time_limit,
                released_pressure=0,
                closed_rate=self.closed_rate,
                closed_valves=deepcopy(self.non_zero_valves),
            )
            next_states = self.next_states

        heap = [state]
        visited: set[int] = set()
        max_pressure = 0
        while len(heap) != 0:
            state = heappop(heap)

            if state.time_remaining <= 0 or state.closed_rate == 0:
                continue

            if state.potential < max_pressure:
                break

            for next_state in next_states(state):
                hsh = hash(next_state)
                if hsh in visited or next_state.potential < max_pressure:
                    continue

                visited.add(hsh)
                if next_state.released_pressure > max_pressure:
                    max_pressure = next_state.released_pressure

                heappush(heap, next_state)

        return max_pressure


@click.command()
@click.argument("fn", type=click.Path(exists=True, dir_okay=False))
def main(fn: str):
    """Solution for day 16

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

    net = Network.from_input(input)
    max_pressure = net.max_pressure_release()
    print(f"Task01: {max_pressure}")

    print("Caution! The Task02 solution is rather slow and takes a couple of minutes.")
    max_pressure_elephant = net.max_pressure_release(with_elephant=True)
    print(f"Task02: {max_pressure_elephant}")


if __name__ == "__main__":
    main()
