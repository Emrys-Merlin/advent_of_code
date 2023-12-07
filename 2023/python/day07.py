from collections import Counter
from pathlib import Path

from loguru import logger
from typer import Typer
from utils import timer

main = Typer()

# Encode card values as hex
# Allows lexicographic sorting on the string
# [-1] is necessary to remove "0x" from e.g. hex(0) == "0x0"
CARD_VALUES = {card: hex(i)[-1] for i, card in enumerate("23456789TJQKA")}
CARD_VALUES_WITH_JOKERS = {card: hex(i)[-1] for i, card in enumerate("J23456789TQKA")}


class Hand:
    """Represent a camel card hand"""

    def __init__(self, hand: str, bid: int, with_jokers: bool = False) -> None:
        """Initialize hand

        hand and bid are saved. The hand is also transformed into a
        hex representation for lexicographic sorting and the card types
        are counted, treating jokers differently if necessary.

        Args:
            hand: the hand
            bid: the bid
            with_jokers: If true, "J" is treated as joker, otherwise as jack. Defaults to False.
        """
        self.hand = hand
        self.bid = bid

        # Count card types separating jokers if necessary
        # Also transform card values to hex
        counts = Counter(hand)
        if with_jokers:
            self.card_values = "".join(CARD_VALUES_WITH_JOKERS[card] for card in hand)
            self.jokers = counts["J"]
            del counts["J"]
        else:
            self.card_values = "".join(CARD_VALUES[card] for card in hand)
            self.jokers = 0

        self.counts = sorted(counts.values(), reverse=True)
        self.type_value = self._type_value()

    def _type_value(self) -> int:
        """Return the type value of the hand

        Jokers are taking into account

        5 cards: 6
        4 cards: 5
        Full house: 4
        3 cards: 3
        2 pairs: 2
        1 pair: 1
        high card: 0
        """
        # Buffer for a hand with only jokers
        # or a hand with only jokers and one other card type
        counts = self.counts + [0] * 2

        if counts[0] + self.jokers == 5:
            return 6

        if counts[0] + self.jokers == 4:
            return 5

        if counts[0] + self.jokers >= 3:
            rem = 3 - counts[0] - self.jokers
            if counts[1] + rem >= 2:
                return 4
            return 3

        if counts[0] + self.jokers >= 2:
            rem = 2 - counts[0] - self.jokers
            if counts[1] + rem >= 2:
                return 2

            return 1

        return 0

    def __repr__(self) -> str:
        """Print hand for debugging."""
        return f"{self.hand} {self.bid}"

    def __lt__(self, other: "Hand") -> bool:
        # If both hands have the same type, we have to compare
        # card values lexicographically
        if self.type_value == other.type_value:
            return self.card_values < other.card_values

        return self.type_value < other.type_value


@timer
def tasks(input: str, with_joker: bool = False) -> int:
    """Solution for tasks 01 and 02

    Args:
        input: Input string
        with_joker: If true treat J as joker, else as jack. Defaults to False.

    Returns:
        Sum over rank*bid of each hand
    """
    hands = []
    for line in input.splitlines():
        hand, bid = line.split()
        hands.append(Hand(hand, int(bid), with_jokers=with_joker))

    return sum(rank * hand.bid for rank, hand in enumerate(sorted(hands), 1))


@main.command()
def entrypoint(path: Path):
    """CLI entrypoint"""
    with open(path, "r") as f:
        input = f.read().strip()

    logger.info(f"Task 01: {tasks(input)}")
    logger.info(f"Task 02: {tasks(input, with_joker=True)}")


if __name__ == "__main__":
    main()
