from dataclasses import dataclass
from functools import total_ordering
from pathlib import Path
from typing import Union

import click

packet = list[Union[int, "packet"]]


@dataclass
@total_ordering
class Packet:
    """Packet with special ordering

    Returns
    -------
    Packet
        The packet for day 13
    """

    pkt: packet

    @staticmethod
    def ordered(pair: tuple[packet, packet]) -> int:
        """Check if two packets are ordered

        Parameters
        ----------
        pair : tuple[packet, packet]
            A pair of two packets

        Returns
        -------
        int
            1 if left < right, -1 if left > right, 0 if 'uncomparable'
        """
        left, right = pair

        for i in range(min(len(left), len(right))):
            left_value = left[i]
            right_value = right[i]

            if isinstance(left_value, int):
                if isinstance(right_value, int):
                    if left_value < right_value:
                        return 1
                    elif left_value > right_value:
                        return -1
                else:
                    res = Packet.ordered(([left_value], right_value))
                    if res == 0:
                        continue
                    return res

            else:
                if isinstance(right_value, int):
                    right_value = [right_value]

                res = Packet.ordered((left_value, right_value))
                if res == 0:
                    continue
                return res

        if len(left) < len(right):
            return 1
        elif len(left) > len(right):
            return -1
        else:
            return 0

    def __lt__(self, other: "Packet") -> bool:
        """Implement <

        Parameters
        ----------
        other : Packet
            Other packet

        Returns
        -------
        bool
            True if self < other
        """
        return Packet.ordered((self.pkt, other.pkt)) == 1

    def __eq__(self, other: object) -> bool:
        """Implement ==

        Parameters
        ----------
        other : object
            Other packet

        Returns
        -------
        bool
            True, if self == other, in the sense that ordered returns 0 (more like an equivalence relation...).
        """
        if not isinstance(other, Packet):
            return NotImplemented
        return Packet.ordered((self.pkt, other.pkt)) == 0


@click.command()
@click.argument("fn", type=click.Path(exists=True, dir_okay=False))
def main(fn: str):
    """Solution for day 13

    FN should point to the input file for today.

    \f
    Parameters
    ----------
    fn : str
        Path to input file
    """
    in_fn = Path(fn)

    pairs: list[tuple[packet, packet]] = []
    all_packets: list[Packet] = [
        Packet([[2]]),
        Packet([[6]]),
    ]
    with open(in_fn, "r") as f:
        text = f.read().split("\n")
        n_blocks = len(text) // 3
        for i in range(n_blocks):
            block = text[3 * i : 3 * (i + 1)]
            left: packet = eval(block[0].strip())
            right: packet = eval(block[1].strip())
            pairs.append((left, right))
            all_packets.append(Packet(left))
            all_packets.append(Packet(right))

    res = 0
    for i, pair in enumerate(pairs, 1):
        if Packet.ordered(pair) == 1:
            res += i

    print(f"Task 01: {res}")

    all_packets.sort()

    idx_2 = 0
    idx_6 = 0
    for i, pkt in enumerate(all_packets, 1):

        if pkt.pkt == [[2]]:
            idx_2 = i
        elif pkt.pkt == [[6]]:
            idx_6 = i

    print(f"Task 02: {idx_2*idx_6}")


if __name__ == "__main__":
    main()
