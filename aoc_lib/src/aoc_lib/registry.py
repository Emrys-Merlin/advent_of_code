import argparse
import importlib.metadata
import inspect
from math import sqrt
import os
from pathlib import Path
import platform
import sys
from time import get_clock_info, perf_counter_ns
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

_TIME_W = 19  # Width of a cell in the output

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
    _ = parser.add_argument("--time", "-t", action="store_true")

    args = parser.parse_args()

    day = cast(int | None, args.day)
    task = cast(int | None, args.task)
    example = cast(int, args.example)
    time = cast(bool, args.time)

    if time:
        runner = _time_solution
        print(_sysinfo())
        print(_row("Day", "Task", "Mean ± STD", "Result"))
    else:
        runner = _run_solution
        print(_row("Day", "Task", None, "Result"))

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
            print(_row(f"{day:02d}", "", "" if time else None, str(e)))
            continue

        for task in tasks:
            solver = _solver_registry.get((day, task))
            line = runner(solver, content, day, task)
            print(line)


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


def _run_solution(
    solver: Solver | None,
    content: str,
    day: int,
    task: int,
) -> str:
    result = solver(content) if solver else "No solver registered."
    return _row(f"{day:02d}", str(task), None, str(result))


def _time_solution(
    solver: Solver | None,
    content: str,
    day: int,
    task: int,
) -> str:
    if solver is None:
        return _row(f"{day:02d}", str(task), "", "No solver registered.")

    time_budget_ns = 10**9  # = 1 s

    total_time = 0
    total_time_square = 0
    counter = 0
    result = ""
    while total_time < time_budget_ns:
        t0 = perf_counter_ns()
        result = solver(content)
        delta = perf_counter_ns() - t0

        total_time += delta
        total_time_square += delta**2
        counter += 1

    mean = total_time / counter
    std = (
        sqrt((counter * total_time_square - total_time**2) / (counter * (counter - 1)))
        if counter > 1
        else None
    )

    duration = _format_duration(mean, std)
    return _row(f"{day:02d}", str(task), duration, str(result))


def _row(day: str, task: str, time: str | None, result: str) -> str:
    cols = [f"{day:>3}", f"{task:>4}"]
    if time is not None:
        cols.append(f"{time:>{_TIME_W}}")
    cols.append(result)
    return "  ".join(cols)


def _format_duration(mean_ns: float, std_ns: float | None = None) -> str:
    scale = 1
    unit = "ns"
    for scale, unit in ((10**9, "s"), (10**6, "ms"), (10**3, "µs"), (1, "ns")):
        if mean_ns >= scale:
            break
    mean = mean_ns / scale
    if std_ns is None:
        return f"{mean:7.2f}          {unit:<2}"
    return f"{mean:7.1f} ± {std_ns / scale:6.1f} {unit:<2}"


def _sysinfo() -> str:
    lines = {
        "Python": _python_info,
        "aoc_lib": lambda: importlib.metadata.version("aoc-lib"),
        "OS": lambda: f"{platform.system()} {platform.release()} {platform.machine()}",
        "CPU": _cpu_info,
        "Memory": _memory_info,
        "Power": _power_info,
        "Timer": _timer_info,
    }
    return "\n".join(
        f"{label:<8} {_best_effort(info)}" for label, info in lines.items()
    )


def _best_effort(info: Callable[[], str]) -> str:
    try:
        return info() or "unknown"
    except Exception:
        return "unknown"


def _python_info() -> str:
    gil = "GIL enabled" if sys._is_gil_enabled() else "GIL disabled"  # pyright: ignore[reportPrivateUsage]
    jit = getattr(sys, "_jit", None)  # 3.14+
    jit_state = "JIT on" if jit is not None and jit.is_enabled() else "JIT off"  # pyright: ignore[reportAny]
    implementation = platform.python_implementation()
    return f"{platform.python_version()} ({implementation}, {gil}, {jit_state})"


def _cpu_info() -> str:
    model = platform.processor() or "unknown"
    try:
        with open("/proc/cpuinfo") as f:
            for line in f:
                if line.startswith("model name"):
                    model = line.split(":", 1)[1].strip()
                    break
    except OSError:
        pass
    return f"{model} · {os.cpu_count()} logical ({os.process_cpu_count()} usable)"


def _memory_info() -> str:
    total = os.sysconf("SC_PAGE_SIZE") * os.sysconf("SC_PHYS_PAGES")
    return f"{total / 2**30:.1f} GiB"


def _power_info() -> str:
    parts: list[str] = []
    try:
        governor = Path("/sys/devices/system/cpu/cpu0/cpufreq/scaling_governor")
        parts.append(f"governor: {governor.read_text().strip()}")
    except OSError:
        pass
    try:
        parts.append("load " + " / ".join(f"{x:.2f}" for x in os.getloadavg()))
    except OSError:
        pass
    return " · ".join(parts)


def _timer_info() -> str:
    clock = get_clock_info("perf_counter")
    return f"perf_counter ({clock.implementation}), resolution {clock.resolution * 1e9:g} ns"
