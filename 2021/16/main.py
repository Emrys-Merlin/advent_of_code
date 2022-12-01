"""Solutions on day 16.

https://adventofcode.com/2021/day/16
"""
from pathlib import Path
from typing import Tuple, Union, List
from collections import namedtuple

import click
from math import prod

Packet = namedtuple("Packet", ["version", "type_id", "payload"])


def hex2bin(hex: str) -> str:
    """Transform hex to binary representation.

    :param hex: String containing hex number
    :returns: String containing binary number
    """
    res = str(bin(int(hex, 16)))[2:]
    # Add missing leading zeros
    residue = (4 - (len(res) % 4)) % 4
    return "0" * residue + res


def parse_packets(packet: str) -> Tuple[Packet, int, int, str]:
    """Parse packets, compute version and computation tree result.

    :param packet: String containing binary representation of packet
    :returns: Tuple containing
    - The parsed packet, which is a named tuplw containing the version
      type_id and the payload (either a number or a list of packets)
    - The total version of subpackets including the current packet
    - The result of the computational subtree
    - The string representation of the unparsed packet remainder
    """
    version, packet = int(packet[:3], 2), packet[3:]
    type_id, packet = int(packet[:3], 2), packet[3:]
    version_sum = 0

    if type_id == 4:
        payload, packet = parse_literal(packet)
        result = payload
    else:
        length_type, packet = packet[0], packet[1:]
        payload = []
        operands = []
        if int(length_type) == 0:
            total_length, packet = int(packet[:15], 2), packet[15:]
            subpackets, packet = packet[:total_length], packet[total_length:]
            while len(subpackets) != 0:
                pld, sub_version_sum, operand, subpackets = parse_packets(subpackets)
                payload.append(pld)
                version_sum += sub_version_sum
                operands.append(operand)
        else:
            n_subpackets, packet = int(packet[:11], 2), packet[11:]
            for _ in range(n_subpackets):
                pld, sub_version_sum, operand, packet = parse_packets(packet)
                payload.append(pld)
                version_sum += sub_version_sum
                operands.append(operand)

        result = apply_operator(type_id, operands)

    pkt = Packet(version, type_id, payload)
    return pkt, version + version_sum, result, packet


def parse_literal(packet: str) -> Tuple[int, str]:
    """Parse literal part of payload.

    :param packet: Packet starting directly at the number part
    :returns: Tuple containing
    - Parsed number
    - String representation of unparsed packet rest
    """
    number = ""
    while True:
        chunk, packet = packet[:5], packet[5:]
        number += chunk[1:]
        if int(chunk[0]) == 0:
            break

    number = int(number, 2)
    return number, packet


OPERATOR_DICT = {
    0: sum,
    1: prod,
    2: min,
    3: max,
    5: lambda x: int(x[0] > x[1]),
    6: lambda x: int(x[0] < x[1]),
    7: lambda x: int(x[0] == x[1]),
}


def apply_operator(type_id: int, operands: List[int]) -> int:
    """Apply operator to operands.

    :param type_id: Operator index
    :param operands: List of operands to apply the operator to
    :returns: Result of operation
    """
    operator = OPERATOR_DICT[type_id]
    return operator(operands)


@click.command()
@click.argument("path", type=click.Path())
def main(path: Union[str, Path]):
    """Solve day 16 tasks.

    Read input from PATH and prints the solution.
    \f

    :param path: Path to the input file
    """
    path = Path(path)

    with open(path, "r") as f:
        hex = f.readline().strip()

    print(f"{hex=}")
    packet = hex2bin(hex)
    # print(f'{packet=}')
    print(f"{len(packet)=}")

    packet_tree, total_version, result, rest = parse_packets(packet)
    # Print whole packet tree and the unparsed packet rest for debug purposes
    # print(packet_tree)
    # print(f'{rest=}')

    print("\nTask 01")
    print(f"{total_version=}")

    print("\nTask 02")
    print(f"{result=}")


if __name__ == "__main__":
    main()
