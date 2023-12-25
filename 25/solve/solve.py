def add_neighbor(
    graph: dict[str, set[str]],
    connected: set[str],
    neighbors: dict[str, int],
    neighbor: str
) -> None:
    connected.add(neighbor)
    for new_neighbor in graph[neighbor]:
        if new_neighbor in connected:
            continue
        if new_neighbor in neighbors:
            neighbors[new_neighbor] += 1
        else:
            neighbors[new_neighbor] = 1

    if neighbor in neighbors:
        del neighbors[neighbor]

def is_isolated(neighbors: dict[str, int], splits: int):
    if len(neighbors) > 3:
        return False
    for neighbor in neighbors:
        if neighbors[neighbor] != 1:
            return False
    return True

def isolate_components(graph: dict[str, set[str]]) -> int:
    first = next(iter(graph.keys()))
    connected = set()
    neighbors = dict()
    add_neighbor(graph, connected, neighbors, first)
    while not is_isolated(neighbors, 3):
        num_added = float("inf")
        for neighbor in neighbors.keys():
            added = len(graph[neighbor] - connected - set(neighbors.keys()))
            if added < num_added:
                least_added = neighbor
                num_added = added

        add_neighbor(graph, connected, neighbors, least_added)

    return len(connected)

if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser("day25 solver")
    parser.add_argument("input", help="Path to the file containing puzzle input")
    args = parser.parse_args()

    with open(args.input) as f:
        graph = dict()
        for line in f:
            kv = line.strip().split(":")
            key = kv[0].strip()
            values = kv[1].strip().split(" ")

            if key not in graph:
                graph[key] = set()
            graph[key].update(set(values))

            for value in values:
                if value not in graph:
                    graph[value] = set()
                graph[value].add(key)

    component_count = isolate_components(graph)
    other_count = len(graph) - component_count
    print(component_count * other_count)
