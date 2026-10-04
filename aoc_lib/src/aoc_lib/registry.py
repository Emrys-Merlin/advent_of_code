import argparse
import inspect
import os
from pathlib import Path
from typing import Callable, Literal, Protocol, cast


class Solver(Protocol):
    def __call__(
        self,
        content: str,
    ) -> str:
        ...


_solver_registry: dict[tuple[int, int], Solver] = {}
_raw_path = os.getenv("AOC_BASE_PATH")
_base_path = Path(_raw_path) if _raw_path else None

RegistryDecorator = Callable[[Solver], Solver]


def solution(day: int, task: Literal[1, 2]) -> RegistryDecorator:
    def register(solver: Solver) -> Solver:
        _solver_registry[(day, task)] = solver
        if _base_path is None:
            _set_base_path(solver)
        return solver

    return register


def solution1(day: int) -> RegistryDecorator:
    """Register the first task of the day."""
    return solution(day, 1)


def solution2(day: int) -> RegistryDecorator:
    """Register the second task of the day."""
    return solution(day, 2)


def cli() -> None:
    parser = argparse.ArgumentParser()
    _ = parser.add_argument("day", type=int, nargs="?", default=None)
    _ = parser.add_argument("task", type=int, nargs="?", default=None, choices=[1, 2])
    _ = parser.add_argument("--example", "-e", type=int, default=0)

    args = parser.parse_args()

    day = cast(int | None, args.day)
    task = cast(int | None, args.task)
    example = cast(int, args.example)

    days = (
        [day]
        if isinstance(day, int)
        else sorted({d for d, _ in _solver_registry.keys()})
    )
    tasks = [task] if isinstance(task, int) else [1, 2]

    for day in days:
        try:
            content = _load_content(day, example)
        except FileNotFoundError as e:
            print(f"Day {day:02d}: {e}")
            continue

        for task in tasks:
            solver = _solver_registry.get((day, task))

            result = solver(content) if solver else "No function registered."
            print(f"Day {day:02d} Task {task}: {result}")


def _load_content(
    day: int,
    example: int,
) -> str:
    if _base_path is None:
        raise FileNotFoundError("Base path not set.")

    if example == 0:
        fn = _base_path / "inputs" / f"day{day:02d}.txt"
    else:
        fn = _base_path / "examples" / f"day{day:02d}_{example:02d}.txt"

    with open(fn) as f:
        return f.read()


def _set_base_path(solver: Solver) -> None:
    global _base_path

    path = Path(inspect.getfile(solver))

    for d in (path, *path.parents):
        if (d / "inputs").is_dir():
            _base_path = d
            return
