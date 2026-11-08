import importlib
from types import ModuleType

import pytest


@pytest.fixture(params=["python", "rust"])
def impl(request: pytest.FixtureRequest) -> ModuleType:
    return importlib.import_module(
        f"aoc_lib.{request.param}.{request.module.IMPL_MODULE}"  # pyright: ignore[reportUnknownMemberType, reportAny]
    )
