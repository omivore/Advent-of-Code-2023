from enum import Enum

class Direction(Enum):
    NORTH = 1
    EAST = 2
    SOUTH = 3
    WEST = 4

Coords = tuple[int, int]
Edge = tuple[Coords, int]

def get_neighbors(forest: [[str]], current: Coords, climbing: bool) -> list[Coords]:
    maxRow = len(forest)
    maxCol = len(forest[0])

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

    paths = []
    for dir in Direction:
        neighbor = neighbors[dir]
        if neighbor[0] < 0 or neighbor[0] >= maxRow or neighbor[1] < 0 or neighbor[1] >= maxCol:
            continue
        dest = forest[neighbor[0]][neighbor[1]]
        if dest == "#" or (not climbing and dest == cant_go[dir]):
            continue
        paths.append(neighbor)
    return paths

def map_nodes(forest: [[str]], climbing: bool) -> dict[Coords, list[Edge]]:
    for i, path in enumerate(forest[0]):
        if path == ".":
            start = i
            break

    nodes = dict()

    seen = set()
    next = [(0, i)]

    while next:
        current = next.pop()
        seen.add(current)
        nodes[current] = map_node(forest, current, climbing)
        for neighbor, _ in nodes[current]:
            if neighbor not in seen:
                next.append(neighbor)

    return nodes

def map_node(
    forest: [[str]],
    current: Coords,
    climbing: bool,
) -> list[Edge]:
    neighbors = get_neighbors(forest, current, climbing)

    junctions = []
    for neighbor in neighbors:
        junctions.append(find_junctions(forest, neighbor, {current}, climbing))
    return junctions

def find_junctions(
    forest: [[str]],
    current: Coords,
    visited: set[Coords],
    climbing: bool,
) -> Edge:
    if current[0] == len(forest) - 1:
        return (current, len(visited))

    neighbors = list(filter(
        lambda n: n not in visited,
        get_neighbors(forest, current, climbing)
    ))

    if len(neighbors) == 1:
        return find_junctions(forest, neighbors[0], visited | {current}, climbing)
    else:
        return (current, len(visited))

def walk(
    nodes: dict[Coords, list[Edge]],
    current: Coords,
    destination: Coords,
    distance: int,
    visited: set[Coords]
) -> int:
    if current == destination:
        return distance

    distances = []
    for neighbor, length in nodes[current]:
        if neighbor in visited:
            continue
        distances.append(walk(nodes, neighbor, destination, distance + length, visited | {current}))

    if distances:
        return max(distances)
    else:
        # dead end self or dead end neighbors
        return 0

def start_walk(forest: [[str]], nodes: dict[Coords, list[Edge]]) -> int:
    for i, path in enumerate(forest[0]):
        if path == ".":
            start = i
            break

    for i, path in enumerate(forest[-1]):
        if path == ".":
            end = i
            break

    return walk(nodes, (0, start), (len(forest) - 1, end), 0, set())


if __name__ == "__main__":
    import argparse

    import sys
    sys.setrecursionlimit(10**6)

    parser = argparse.ArgumentParser("day23 solver")
    parser.add_argument("input", help="Path to the file containing puzzle input")
    parser.add_argument("--climb", default=False, action="store_true", help="Indicate that slopes can be climbed")
    args = parser.parse_args()

    with open(args.input) as f:
        lines = [list(line.rstrip()) for line in f]
        node_map = map_nodes(lines, args.climb)
        longest = start_walk(lines, node_map)
        print(longest)
