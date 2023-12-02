from datetime import datetime as dt
from datetime import timedelta
from typing import Callable

from loguru import logger


def format_timedelta(delta: timedelta) -> str:
    """Format timedelta ojbect to HH:MM:SS.ssssss"""
    seconds = delta.total_seconds()
    hours, rem = divmod(seconds, 60 * 60)
    minutes, seconds = divmod(rem, 60)
    hours = int(hours)
    minutes = int(minutes)

    return f"{hours:02d}:{minutes:02d}:{seconds:013.10f}"


def timer(func: Callable) -> Callable:
    """Decorator to time function execution time

    Execution time will be printed before function output.
    """

    def inner(*args, **kwargs):
        start = dt.now()
        res = func(*args, **kwargs)
        end = dt.now()
        delta = end - start
        logger.info("Execution time: " + format_timedelta(delta))
        return res

    return inner
