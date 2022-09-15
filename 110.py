# Below implementation proudly stolen from: https://github.com/CarlosLunaMota/Rule110

# rule110 survivability pattern https://en.wikipedia.org/wiki/Rule_110
# current pattern  111 110 101 100 011 010 001 000
# new cell          0   1   1   0   1   1   1   0
def rule110(universe):
    """Performs a single iteration of the Rule 110 in a borderless universe."""

    new = set()
    for x in universe:
        if x + 1 not in universe:
            new.add(x)
        if x - 1 not in universe:
            new.add(x)
            new.add(x - 1)
    return new


def show(universe, window, alive='1', dead='0', space=''):
    """Prints a segment of the universe on the screen."""
    print(space.join(alive if x in universe else dead for x in range(*window)))


if __name__ == "__main__":
    generations = 20  # Number of iterations that will be shown
    universe = {generations - 1}  # A set containing the indices of living cells
    window = (0, generations)  # The segment of the universe that will be shown
    for i in range(generations):
        show(universe, window)
        universe = rule110(universe)
