import copy
from typing import Optional

def step(map: [[str]], row: int, col: int) -> str:
    if map[row][col] == "#":
        return "#"

    neighbors = get_neighbors(map, row, col)
    if "O" in neighbors:
        return "O"
    else:
        return "."

def get_neighbors(map: [[str]], row: int, col: int) -> tuple[Optional[str], Optional[str], Optional[str], Optional[str]]:
    up = map[row - 1][col] if row - 1 >= 0 else None
    down = map[row + 1][col] if row + 1 < len(map) else None
    left = map[row][col - 1] if col - 1 >= 0 else None
    right = map[row][col + 1] if col + 1 < len(map[0]) else None

    return up, down, left, right

def walk(map: [[str]], steps: int) -> [str]:
    for row in range(len(map)):
        for col in range(len(map[row])):
            if map[row][col] == "S":
                map[row][col] = "O"

    this_map = copy.deepcopy(map)
    for _ in range(steps):
        next_map = copy.deepcopy(this_map)
        for row in range(len(this_map)):
            for col in range(len(this_map[row])):
                next_map[row][col] = step(this_map, row, col)
        this_map = next_map

    return next_map

def print_map(map: [[str]]) -> None:
    for row in map:
        print("".join(row))


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser("day21 solver")
    parser.add_argument("input", help="Path to the file containing puzzle input")
    parser.add_argument("steps", help="Number of steps to take")
    args = parser.parse_args()

    with open(args.input) as f:
        lines = [list(line.rstrip()) for line in f]
        final_map = walk(lines, int(args.steps))
        sum = 0
        for line in final_map:
            sum += line.count("O")
        print(sum)
