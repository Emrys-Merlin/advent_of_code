from importlib import import_module
from pathlib import Path
from typing import Annotated, Callable

from typer import Option, Typer, echo

main = Typer()

ROOT = Path(__file__).parent.parent.parent.parent


@main.command()
def run(
    day: int,
    task: int,
    example: Annotated[int, Option("--example", "-e")] = 0,
):
    if example != 0:
        echo(f"Running example {example} for day {day} task {task}")
        fn = ROOT / "examples" / f"day{day:02d}_{example:02d}.txt"

    else:
        echo(f"Running day {day} task {task}")
        fn = ROOT / "inputs" / f"day{day:02d}.txt"

    with open(fn, "r") as f:
        data = f.read()

    echo(data)

    module = import_module(f"aoc2024.day{day:02d}")
    task_fn: Callable[[str], str] = getattr(module, f"task{task:02d}")
    result = task_fn(data)
    echo(f"Result: {result}")


if __name__ == "__main__":
    main()
