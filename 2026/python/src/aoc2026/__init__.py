"""Importing the package registers every solver via its decorator."""

from aoc_lib.registry import cli

from . import (
    task01,
    task02,
    task03,
    task04,
    task05,
    task06,
    task07,
    task08,
    task09,
    task10,
    task11,
    task12,
)

__all__ = [
    "cli",
    "task01",
    "task02",
    "task03",
    "task04",
    "task05",
    "task06",
    "task07",
    "task08",
    "task09",
    "task10",
    "task11",
    "task12",
]
