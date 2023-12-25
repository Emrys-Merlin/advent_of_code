from dataclasses import dataclass
from fractions import Fraction
from pathlib import Path
from typing import Union

from loguru import logger
from typer import Typer
from utils import timer

main = Typer()


@dataclass(frozen=True)
class Vector:
    """3D vector class (over rational numbers)"""

    x: Fraction = Fraction(0)
    y: Fraction = Fraction(0)
    z: Fraction = Fraction(0)

    def __add__(self, other: "Vector") -> "Vector":
        return Vector(self.x + other.x, self.y + other.y, self.z + other.z)

    def __sub__(self, other: "Vector") -> "Vector":
        return Vector(self.x - other.x, self.y - other.y, self.z - other.z)

    def __mul__(self, other: int) -> "Vector":
        return Vector(self.x * other, self.y * other, self.z * other)

    def __abs__(self) -> int:
        return abs(self.x) + abs(self.y) + abs(self.z)

    def dot(self, other: "Vector") -> Fraction:
        """Standard scalar (or dot) product"""
        return self.x * other.x + self.y * other.y + self.z * other.z

    def collinear(self, other: "Vector") -> bool:
        """Check if two vectors are collinear"""
        if all([self.x == 0, self.y == 0, self.z == 0]) or all(
            [other.x == 0, other.y == 0, other.z == 0]
        ):
            return True
        factors = set()
        for attr in ["x", "y", "z"]:
            s = getattr(self, attr)
            o = getattr(other, attr)
            if (s == 0 and o != 0) or (s != 0 and o == 0):
                return False
            if s == o == 0:
                continue
            factors.add(s / o)

        return len(factors) == 1

    def cross(self, other: "Vector") -> "Vector":
        """Cross product of two vectors

        Assumes vectors are not collinear

        Args:
            other: Other vector

        Returns:
            Orthogonal vector to both vectors, with length equal to the area of the
            parallelogram defined by the two vectors
        """
        return Vector(
            self.y * other.z - self.z * other.y,
            -(self.x * other.z - self.z * other.x),
            self.x * other.y - self.y * other.x,
        )


@dataclass(frozen=True)
class Line:
    """Line in 3D space represented by base and direction"""

    base: Vector
    direction: Vector

    def __contains__(self, point: Vector) -> bool:
        return (point - self.base).collinear(self.direction)

    @classmethod
    def from_input_line(cls, line: str) -> "Line":
        left, right = line.split("@")
        base = Vector(*[Fraction(x.strip()) for x in left.split(",")])
        direction = Vector(*[Fraction(x.strip()) for x in right.split(",")])

        return cls(base, direction)

    @classmethod
    def from_points(
        cls,
        point1: Vector,
        point2: Vector,
        t1: int | Fraction = 0,
        t2: int | Fraction = 1,
    ) -> "Line":
        """Get line through two points

        If time points are specified, the resulting line will be parametrized such that
        l(t1) = point1 and l(t2) = point2.

        Args:
            point1: point1
            point2: point2
            t1: Optional time1. Defaults to 0.
            t2: Optional time2. Defaults to 1.

        Returns:
            Line through points
        """
        direction = (point2 - point1) * Fraction(1, t2 - t1)
        base = point1 - direction * t1
        return cls(base, direction)

    def __call__(self, t: int | Fraction) -> Vector:
        return self.base + self.direction * t

    def _2d_intersection(
        self,
        other: "Line",
        only_future: bool = False,
    ) -> Union[Vector, "Line", None]:
        """Compute 2D intersection of lines

        This method implements Cramer's rule for 2D linear equations.

        Args:
            other: Line
            only_future: Only return intersection if it lies in the future, i.e.
                t >= 0 and s >= 0 where self(t) and other(s).

        Returns:
            Line if lines are identical, None if lines are parallel, Vector if lines intersect
        """
        b = other.base - self.base

        det = (
            -self.direction.x * other.direction.y + self.direction.y * other.direction.x
        )

        if det == 0:
            if other.base in self:
                return self

            return None

        t = -other.direction.y * b.x + other.direction.x * b.y
        s = -self.direction.y * b.x + self.direction.x * b.y
        t /= det
        s /= det

        if only_future and (t < 0 or s < 0):
            return None

        res = self.base + self.direction * t
        return Vector(res.x, res.y)

    def _minimal_distance(
        self, other: "Line", approx: bool = False
    ) -> tuple[Vector, Vector]:
        """Compute the closest points on two lines

        Solve the system of equations by setting the first derivative to 0 of
        |l1(t) - l2(s)|^2.

        Args:
            other: other line
            approx: Round computed times to closest integer. Defaults to False.

        Returns:
            (Approximately) closest points
        """
        # Minimize ||l1(t) - l2(s)||^2
        # Checking only first derivative is 0
        v1 = self.direction.dot(self.direction)
        v2 = other.direction.dot(other.direction)
        v1v2 = -self.direction.dot(other.direction)
        b1 = -self.direction.dot(self.base - other.base)
        b2 = other.direction.dot(self.base - other.base)

        det = v1 * v2 - v1v2**2
        t = (v2 * b1 - v1v2 * b2) / det
        s = (v1 * b2 - v1v2 * b1) / det

        if approx:
            t = round(t)
            s = round(s)

        return self(t), other(s)

    def closest_points(
        self, other: "Line", approx: bool = False
    ) -> tuple[Vector, Vector]:
        """Compute the closest points on two lines

        Solve the system of equations by setting the first derivative to 0 of
        |l1(t) - l2(s)|^2.

        Args:
            other: other line
            approx: Round computed times to closest integer. Defaults to False.

        Returns:
            (Approximately) closest points
        """
        if self.direction.collinear(other.direction):
            return self.base, other.base

        return self._minimal_distance(other, approx=approx)

    def _3d_intersection(self, other: "Line") -> Union[Vector, "Line", None]:
        """Compute 3D intersection of lines

        First check if lines intersect at all. If they do, compute the closest points
        on the lines, which must be the intersection point.
        """
        if not self.intersect(other):
            return None

        point1, point2 = self.closest_points(other)

        if point1 != point2:
            message = f"{point1=} != {point2=}"
            raise Exception(message)

        return point1

    def intersection(
        self,
        other: "Line",
        only_future: bool = False,
        ignore_z: bool = False,
    ) -> Union[Vector, "Line", None]:
        """Compute intersection of two lines

        only_future is only respected for ignore_z=True

        Args:
            other: other line
            only_future: Only return intersection if it lies in the future, i.e.
                t >= 0 and s >= 0 where self(t) and other(s).
            ignore_z: Ignore z coordinate. Defaults to False.

        Returns:
            Intersection point
        """
        if ignore_z:
            return self._2d_intersection(other, only_future=only_future)

        return self._3d_intersection(other)

    def intersect(self, other: "Line") -> bool:
        """Check if two lines intersect without computing intersection point"""
        if self.direction.collinear(other.direction):
            return other.base in self

        normal = self.direction.cross(other.direction)
        return normal.dot(self.base - other.base) == 0

    def construct_plane(self, other: "Line") -> Union[None, "Plane"]:
        """Construct plane from two lines

        If the two lines intersect or are parallel, return the spanned
        plane. If the lines are skewed or identical, return None.

        Args:
            other: other line

        Returns:
            Spanned plane or None
        """
        if self.direction.collinear(other.direction):
            # The lines are identical
            if other.base in self:
                return None
            # The lines are parallel
            return Plane(self.base, self.direction.cross(other.base - self.base))

        # The lines are skewed
        if not self.intersect(other):
            return None
        # The lines intersect
        return Plane(self.base, self.direction.cross(other.direction))

    def closest_point(
        self, point: Vector, approx: bool = False
    ) -> tuple[Fraction, Vector]:
        """Compute closest point on line to given point.

        If approx is true, round time step to nearest integer.
        """
        delta = point - self.base
        t = delta.dot(self.direction) / self.direction.dot(self.direction)
        if approx:
            t = round(t.denominator)
        closest = self(t)
        return t, closest


@dataclass(frozen=True)
class Plane:
    """Plane in 3d space represented by base and normal vector"""

    base: Vector
    normal: Vector

    @classmethod
    def from_line_and_point(cls, line: Line, point: Vector) -> "Plane":
        if point in line:
            raise ValueError("Point is on the line")
        return cls(line.base, line.direction.cross(point - line.base))

    def __contains__(self, point: Vector) -> bool:
        return self.normal.dot(point - self.base) == 0

    def intersection(self, other: "Plane") -> Union["Plane", "Line", None]:
        """Compute intersection with other plane.

        Args:
            other: other plane

        Returns:
            Plane if planes are identical, None if planes are paralle, and line if planes intersect
        """
        if self.normal.collinear(other.normal):
            if other.base in self:
                return self
            return None

        direction = self.normal.cross(other.normal)
        # Taken from https://math.stackexchange.com/a/3864528
        q = -other.normal.cross(other.normal.cross(self.normal))
        r = -self.normal.cross(self.normal.cross(other.normal))
        base = self.base.dot(self.normal) * q + other.base.dot(other.normal) * r
        return Line(base, direction)

    def intersection_line(self, line: Line) -> Line | Vector | None:
        """Intersect plane with line

        Args:
            line: other line

        Returns:
            Line if line is in plane, None if line is parallel to plane, and vector if line intersects plane
        """
        if line.direction.dot(self.normal) == 0:
            if line.base in self:
                return line
            return None

        t = self.normal.dot(self.base - line.base) / self.normal.dot(line.direction)
        return line(t)


def reparametrize(candidate: Line, line1: Line, line2: Line) -> Line:
    """Reparametrize line such that it intersects line1 and line2 at correct time steps

    Args:
        candidate: Candidate intersecting both line1 and line2
        line1: Other line
        line2: Other line

    Returns:
        Reparametrized candidate line
    """
    p1 = candidate.intersection(line1)
    t1 = line1.closest_point(p1)[0]
    p2 = candidate.intersection(line2)
    t2 = line2.closest_point(p2)[0]
    return Line.from_points(p1, p2, t1, t2)


def check_time_step(t: Fraction, lines: list[Line]) -> tuple[int, Line | None]:
    """Magic method candidate intersects all lines

    We only use four lines, because these already define the intersecting line.
    If the problem is solvable, the intersecting line must be the candidate.

    We exploit that all lines are in general position (for this input)
    We take a point on line1 by evaluating at timestep t. We then construct a plane
    through this point and line2. We then check if line3 intersects this plane (which it does
    because of general position) and use the intersection point to construct a candidate line through
    point1 and the intersection. Then we compute the intersection of line 4 with the plane, called intersection2.

    There are 3 cases:
        1. intersection1 is on the candidate line -> we are done and return 0 +  the candidate line
        2. intersection1 is not on the one candidate line -> we return -1 + None
        3. intersection1 is not on the other candidate line -> we return 1 + None

    The sign is determined by the normal of the candidate sign in the plane, which is computed consistently
    between timesteps.

    Key idea:
    This sign method allows us to use binary search to find the correct timestep.

    Args:
        t: Time step to check
        lines: Lines to check (only first four are used

    Raises:
        NotImplementedError: If line3 or line4 do not intersect the plane in a point

    Returns:
        See above
    """
    assert len(lines) >= 4

    # Switching the first two lines fixes problem with parallel lines in example
    line1 = lines[1]
    line2 = lines[0]
    line3 = lines[2]
    line4 = lines[3]

    point = line1(t)
    plane = Plane.from_line_and_point(line2, point)

    intersection1 = plane.intersection_line(line3)

    if not isinstance(intersection1, Vector):
        raise NotImplementedError("Intersection is a line")

    candidate_line = Line.from_points(point, intersection1)

    normal_in_plane = plane.normal.cross(candidate_line.direction)

    intersection2 = plane.intersection_line(line4)

    if not isinstance(intersection2, Vector):
        raise NotImplementedError("Intersection is a line")

    res = normal_in_plane.dot(intersection2 - point)
    if res < 0:
        return -1, None
    elif res > 0:
        return 1, None

    return 0, candidate_line


def find_intesecting_line(lines: list[Line], t_hi_min: int = 1_000_000_000) -> Line:
    """Perform binary search to fin the intersecting line

    See check_time_step for details.

    For each time step, we can figure out if a test point is to the left or the right
    of a candidate line (in an appropriate orientation). Hence, we can half intervals
    until we find the correct time step.

    Args:
        lines: Lines to check (only first four are used)
        t_hi_min: Minimum t_hi that we check for the test point being on the other side
        of the candidate line. Defaults to 1_000_000_000.

    Raises:
        ValueError: If we cannot find a line that intersects all lines (on integer time steps)

    Returns:
        Intersection line in correct parametrization
    """
    t_lo = 1
    sign_lo, _ = check_time_step(t_lo, lines)
    t_hi = t_hi_min

    while check_time_step(t_hi, lines)[0] == sign_lo:
        t_hi *= 2

    while t_hi - t_lo > 1:
        t_mid = (t_lo + t_hi) // 2
        sign_mid, candidate_line = check_time_step(t_mid, lines)

        if sign_mid == 0 and isinstance(candidate_line, Line):
            return reparametrize(candidate_line, lines[0], lines[1])

        if sign_mid == sign_lo:
            t_lo = t_mid
        else:
            t_hi = t_mid

    raise ValueError("No intersecting line found (at integer time steps)")


def parse_input(input: str) -> list[Line]:
    """Parse input

    Args:
        input: Input string

    Returns:
        List of lines
    """
    return [Line.from_input_line(line) for line in input.splitlines()]


@timer
def task01(
    input: str,
    lo: int = 200_000_000_000_000,
    hi: int = 400_000_000_000_000,
) -> int:
    """Compute 2D line intersections in rectangle

    Args:
        input: Input string
        lo: top/left side of rectangle. Defaults to 200_000_000_000_000.
        hi: bottom/right side of rectangle. Defaults to 400_000_000_000_000.

    Returns:
        Number of intersections in rectangle
    """
    lines = parse_input(input)

    counter = 0
    for i, line in enumerate(lines[:-1]):
        for other in lines[i + 1 :]:
            intersection = line.intersection(other, ignore_z=True, only_future=True)
            if intersection is None:
                continue

            # TODO Does not hold in general, but works for this input
            if isinstance(intersection, Line):
                counter += 1
                continue

            if lo <= intersection.x <= hi and lo <= intersection.y <= hi:
                counter += 1

    return counter


@timer
def task02(input: str, t_hi_min: int = 1_000_000_000) -> int:
    """Find line that intersects all other lines

    Args:
        input: Input string
        t_hi_min: Minimum t_hi for binary search. Defaults to 1_000_000_000.

    Returns:
        x + y + z coordinate of base of intersecting line (in correct parametrization)
    """
    lines = parse_input(input)

    intersecting_line = find_intesecting_line(lines, t_hi_min=t_hi_min)
    base = intersecting_line.base

    return base.x + base.y + base.z


@main.command()
def entrypoint(path: Path):
    """CLI entrypoint"""
    with open(path, "r") as f:
        data = f.read().strip()

    logger.info(f"Task01: {task01(data)}")
    logger.info(f"Task02: {task02(data)}")


if __name__ == "__main__":
    main()
