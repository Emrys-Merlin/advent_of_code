"""Solutions on day 19.

https://adventofcode.com/2021/day/19
"""
from pathlib import Path
from typing import List, Tuple, Union, Dict
from collections import defaultdict

import click
import numpy as np


class Scanner:
    """Encapsulate scanners and beacons."""

    def __init__(self, beacons: np.ndarray):
        """Initialize scanner.

        :param beacons: List of beacon coordinates.
        """
        self.beacons = beacons
        self.beacon_signature = defaultdict(set)
        self.scanner_locations = np.zeros((1, 3), dtype='int')

        for i in range(len(beacons) - 1):
            beacon1 = beacons[i]
            for j in range(i+1, len(beacons)):
                beacon2 = beacons[j]

                delta = beacon2 - beacon1
                delta = tuple(sorted(np.abs(delta).tolist()))

                self.beacon_signature[i].add(
                    delta
                )
                self.beacon_signature[j].add(
                    delta
                )

    def intersection(self, scanner: 'Scanner') -> Dict[int, int]:
        """Compute beacon intersection between two scanners.

        :param scanner: Other scanner
        :returns: Dictionary containing mapping between matching
        beacons
        """
        matches = {}
        for i, bc1 in self.beacon_signature.items():
            max_intersection = 0
            idx = -1
            for j, bc2 in scanner.beacon_signature.items():
                n_intersection = len(bc1.intersection(bc2))
                if n_intersection > max_intersection:
                    idx = j
                    max_intersection = n_intersection

            if max_intersection >= 11:
                matches[i] = idx

        return matches

    def extend(self, scanner: 'Scanner', matches: Dict[int, int]):
        """Extend scanner by beacon list of second scanner.

        Compute coordinate transformation and extend first
        scanner by beacons of second scanner. Add scanner locations
        of second scanner to scanner locations of first scanner
        in the coordinate system of first scanner.

        :param scanner: Other scanner
        :param matches: Mapping between matched beacons
        """
        # Find coordinate transformation
        y = self.beacons[list(matches.keys())]
        indices = list(matches.values())
        x = scanner.beacons[indices]
        trafo = Scanner._fit_transformation(x, y)

        # Transform missing new beacons
        residuals = list(set(range(len(scanner.beacons))).difference(indices))
        x = scanner.beacons[residuals]
        new_beacons = Scanner._transform_coords(trafo, x)

        # Extend beacon list
        n = len(self.beacons)
        self.beacons = np.concatenate((self.beacons, new_beacons), axis=0)

        # Extend internal beacon signatures of second scanner
        for j, i in enumerate(residuals, n):
            self.beacon_signature[j] = scanner.beacon_signature[i]

        # Compute cross signatures between scanner 1 and scanner 2
        for i, beacon1 in enumerate(self.beacons):
            for j, beacon2 in enumerate(new_beacons, n):
                # print(j)
                delta = beacon2 - beacon1
                delta = tuple(sorted(np.abs(delta).tolist()))

                self.beacon_signature[i].add(
                    delta
                )
                self.beacon_signature[j].add(
                    delta
                )

        # Add new scanner locations in correct coordinate system
        locations = Scanner._transform_coords(trafo, scanner.scanner_locations)
        self.scanner_locations = np.concatenate(
            (self.scanner_locations, locations),
            axis=0
        )

    @staticmethod
    def _fit_transformation(x: np.ndarray, y: np.ndarray) -> np.ndarray:
        """Fit affine coordinate transformation.

        :param x: Input coordinates
        :param y: Output coordinates
        :returns: Transformation matrix
        """
        x = np.concatenate((x, np.ones((len(x), 1))), axis=-1)
        trafo, _, _, _ = np.linalg.lstsq(x, y, rcond=None)
        return trafo

    @staticmethod
    def _transform_coords(trafo: np.ndarray, x: np.ndarray) -> np.ndarray:
        """Apply affine coordinate transformation.

        :param trafo: Transformation matrix
        :param x: Input coordinates
        :param y: Transformed coordinates
        """
        x = np.concatenate((x, np.ones((len(x), 1))),
                           axis=-1)
        return np.round(x@trafo).astype(int)

    def __len__(self) -> int:
        """Return number of beacons."""
        return len(self.beacons)

    def max_scanner_distance(self) -> int:
        """Return maximum L1 scanner distance."""
        max_dist = -1
        n = len(self.scanner_locations)
        for i in range(n - 1):
            loc1 = self.scanner_locations[i]
            for j in range(i+1, n):
                loc2 = self.scanner_locations[j]

                dist = np.abs(loc1 - loc2).sum()
                if dist > max_dist:
                    max_dist = dist

        return max_dist

    @staticmethod
    def solve_tasks(scanners: List['Scanner']) -> Tuple[int, int]:
        """Compute number of beacons and scanner distance.

        :param scanners: List of all scanners
        :returns: Tuple of number of beacons and maximum L1 scanner distance
        """
        base_scanner = scanners[0]
        scanners = scanners[1:]

        while len(scanners):
            remaining_scanners = []
            for scanner in scanners:
                matches = base_scanner.intersection(scanner)
                if len(matches) >= 12:
                    base_scanner.extend(scanner, matches)
                    continue
                remaining_scanners.append(scanner)

            scanners = remaining_scanners

        return len(base_scanner), base_scanner.max_scanner_distance()


@click.command()
@click.argument('path', type=click.Path())
def main(path: Union[str, Path]):
    """Solve day 19 tasks.

    Read input from PATH and prints the solution.
    \f

    :param path: Path to the input file
    """
    path = Path(path)

    scanners = []
    beacons = []
    with open(path, 'r') as f:
        for line in f.readlines():
            line = line.strip()
            if len(line) == 0:
                beacons = np.array(beacons, dtype='int')
                scanner = Scanner(beacons)
                scanners.append(scanner)
                beacons = []
                continue

            if not line.startswith('---'):
                beacons.append([
                    int(number)
                    for number in line.split(',')
                ])

    if len(beacons) != 0:
        beacons = np.array(beacons, dtype='int')
        scanner = Scanner(beacons)
        scanners.append(scanner)

    print(f'{len(scanners)=}')

    n_beacons, max_scanner_distance = Scanner.solve_tasks(scanners)

    print('\nTask 01')
    print(f'{n_beacons=}')

    print('\nTask 02')
    print(f'{max_scanner_distance=}')


if __name__ == '__main__':
    main()
