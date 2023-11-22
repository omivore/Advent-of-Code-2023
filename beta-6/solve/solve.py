def func(x):
    return x + 1

def are_all_unique(string: str) -> bool:
    return len(string) == len(set(string))

def detect_packet_start(stream: str) -> int:
    index = 4
    candidate = stream[:4]
    remaining = stream[4:]

    while not are_all_unique(candidate):
        new_token, remaining = remaining[:1], remaining[1:]
        candidate = candidate[1:] + new_token
        index += 1

    return index

if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser("beta-6 solver")
    parser.add_argument("input", help="Path to the file containing puzzle input")
    args = parser.parse_args()

    with open(args.input) as f:
        for line in f:
            print(detect_packet_start(line))
