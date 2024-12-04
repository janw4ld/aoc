# pylint: disable=missing-module-docstring,missing-function-docstring
lines = []
# with open("test-input", "r", encoding="utf8") as file:
with open("input", "r", encoding="utf8") as f:
    lines = f.read().strip().split("\n")

DELTAS = [(0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (-1, -1), (1, -1), (-1, 1)]
ANCHOR = "X"
STEPS = {"M": 1, "A": 2, "S": 3}


def scan_occurrences(x_x, x_y):
    return sum(  # implicit cast to int
        all(
            0 <= x_x + d_x * s < len(lines[0])
            and 0 <= x_y + d_y * s < len(lines)
            and lines[x_y + d_y * s][x_x + s * d_x] == char
            for char, s in STEPS.items()
        )
        for (d_x, d_y) in DELTAS
    )


part1 = sum(
    scan_occurrences(x, y)
    for y, line in enumerate(lines)
    for x, char in enumerate(line)
    if char == ANCHOR
)

print(part1)
