from enum import Enum

class Direction(Enum):
    NORTH = 1
    EAST = 2
    SOUTH = 3
    WEST = 4

def walk(map: [[str]], current: tuple[int, int], visited: set[tuple[int, int]]):
    maxRow = len(map)
    maxCol = len(map[0])

    if current[0] == maxRow - 1:
        return len(visited)

    neighbors = {
        Direction.NORTH: (current[0] - 1, current[1]),
        Direction.EAST: (current[0], current[1] + 1),
        Direction.SOUTH: (current[0] + 1, current[1]),
        Direction.WEST: (current[0], current[1] - 1),
    }

    cant_go = {
        Direction.NORTH: "v",
        Direction.EAST: "<",
        Direction.SOUTH: "^",
        Direction.WEST: ">",
    }

    distances = []
    for dir in Direction:
        neighbor = neighbors[dir]
        if neighbor in visited:
            continue
        if neighbor[0] < 0 or neighbor[0] >= maxRow or neighbor[1] < 0 or neighbor[1] >= maxCol:
            continue
        dest = map[neighbor[0]][neighbor[1]]
        if dest == "#" or dest == cant_go[dir]:
            continue
        distances.append(walk(map, neighbor, visited | {current}))

    return max(distances)

def start_walk(map: [[str]]) -> int:
    for i, path in enumerate(map[0]):
        if path == ".":
            start = i
            break

    return walk(map, (0, start), set())


if __name__ == "__main__":
    import argparse

    import sys
    sys.setrecursionlimit(10**6)

    parser = argparse.ArgumentParser("day21 solver")
    parser.add_argument("input", help="Path to the file containing puzzle input")
    #parser.add_argument("steps", help="Number of steps to take")
    args = parser.parse_args()

    with open(args.input) as f:
        lines = [list(line.rstrip()) for line in f]
        longest = start_walk(lines)
        print(longest)
