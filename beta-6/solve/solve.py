def func(x):
    return x + 1

if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser("beta-6 solver")
    parser.add_argument("input", help="Path to the file containing puzzle input")
    args = parser.parse_args()
    print(args.input)
