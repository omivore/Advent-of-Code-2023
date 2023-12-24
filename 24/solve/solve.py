from decimal import *
from itertools import combinations

getcontext().prec = 32

class Point:
    def __init__(self, x: int, y: int, z: int) -> None:
        self.x = x
        self.y = y
        self.z = z

    def __repr__(self) -> str:
        return f"({self.x}, {self.y}, {self.z})"

class Vector:
    def __init__(self, input: str, override_z: None | int = None) -> None:
        halves = input.split("@")

        intercepts = halves[0].strip().split(", ")
        self.intercept = Point(
            Decimal(intercepts[0]),
            Decimal(intercepts[1]),
            override_z if override_z else Decimal(intercepts[2]),
        )

        slopes = halves[1].strip().split(", ")
        self.x = Decimal(slopes[0])
        self.y = Decimal(slopes[1])
        self.z = 0 if override_z else Decimal(slopes[2])

    def at(self, time: int) -> Point:
        return Point(
            self.x * time + self.intercept.x,
            self.y * time + self.intercept.y,
            self.z * time + self.intercept.z,
        )

    def intersects_between(self, other: "Vector", axis: str, lower: int, upper: int) -> tuple[bool, tuple[int, int, int, int]]:
        self_lower_time = (lower - getattr(self.intercept, axis)) / getattr(self, axis)
        self_lower = self.at(self_lower_time)
        self_upper_time = (upper - getattr(self.intercept, axis)) / getattr(self, axis)
        self_upper = self.at(self_upper_time)

        other_lower_time = (lower - getattr(other.intercept, axis)) / getattr(other, axis)
        other_lower = other.at(other_lower_time)
        other_upper_time = (upper - getattr(other.intercept, axis)) / getattr(other, axis)
        other_upper = other.at(other_upper_time)

        result = Vector.intersected(self_lower, other_lower, self_upper, other_upper)
        return (result, (self_lower_time, self_upper_time, other_lower_time, other_upper_time))

    def crosses(self, other: "Vector", lower: int, upper: int) -> bool:
        result_x, times_x = self.intersects_between(other, "x", lower, upper)
        if not result_x: return False
        if all(map(lambda time: time < 0, times_x[:2])) or all(map(lambda time: time < 0, times_x[2:])): return False

        result_y, times_y = self.intersects_between(other, "y", lower, upper)
        if not result_y: return False
        if all(map(lambda time: time < 0, times_y[:2])) or all(map(lambda time: time < 0, times_y[2:])): return False

        if this.z != 0:
            result_z, times_z = self.intersects_between(other, "z", lower, upper)
            if not result_z: return False
            if all(map(lambda time: time < 0, times_z[:2])) or all(map(lambda time: time < 0, times_z[2:])): return False

        if (
            all(map(lambda time: time > 0, times_x))
            and all(map(lambda time: time > 0, times_y))
            and (this.z == 0 or all(map(lambda time: time > 0, times_z)))
        ):
            return True

        # Otherwise, at least one of them is a partial.
        x_live_self_bounds = (max(lower, self.intercept.x), upper) if self.x > 0 else (lower, min(upper, self.intercept.x))
        result_x_self, _ = self.intersects_between(other, "x", *x_live_self_bounds)

        x_live_other_bounds = (max(lower, other.intercept.x), upper) if other.x > 0 else (lower, min(upper, other.intercept.x))
        result_x_other, _ = self.intersects_between(other, "x", *x_live_other_bounds)

        y_live_self_bounds = (max(lower, self.intercept.y), upper) if self.y > 0 else (lower, min(upper, self.intercept.y))
        result_y_self, _ = self.intersects_between(other, "y", *y_live_self_bounds)

        y_live_other_bounds = (max(lower, other.intercept.y), upper) if other.y > 0 else (lower, min(upper, other.intercept.y))
        result_y_other, _ = self.intersects_between(other, "y", *y_live_other_bounds)

        if result_x_self and result_x_other and result_y_self and result_y_other:
            return True
        else:
            return False

    def intersected(
        before_a: Point,
        before_b: Point,
        after_a: Point,
        after_b: Point
    ) -> bool:
        before_diff_x = before_a.x - before_b.x
        before_diff_y = before_a.y - before_b.y
        before_diff_z = before_a.z - before_b.z

        after_diff_x = after_a.x - after_b.x
        after_diff_y = after_a.y - after_b.y
        after_diff_z = after_a.z - after_b.z

        return (
            (before_diff_x * after_diff_x <= Decimal(1e-10))
            and (before_diff_y * after_diff_y <= Decimal(1e-10))
            and (before_diff_z * after_diff_z <= Decimal(1e-10))
        )

    def __repr__(self) -> str:
        return f"{self.intercept}[{self.x}, {self.y}, {self.z}]"



if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser("day24 solver")
    parser.add_argument("input", help="Path to the file containing puzzle input")
    #parser.add_argument("--climb", default=False, action="store_true", help="Indicate that slopes can be climbed")
    args = parser.parse_args()

    with open(args.input) as f:
        vectors = [Vector(line.rstrip(), Decimal(3e14)) for line in f]
        sum = 0
        for this, that in combinations(vectors, 2):
            if this.crosses(that, Decimal(2e14), Decimal(4e14)):
                sum += 1
        print(sum)
