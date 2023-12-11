package maps

import (
    "bufio"
    "strings"
    "testing"
)

func CheckArray[S ~[]E, E comparable](t *testing.T, expected S, got S) {
    if len(got) != len(expected) {
        t.Errorf("Expected %d elements, got %d elements.", len(expected), len(got))
    }
    for i := range expected {
        if got[i] != expected[i] {
            t.Errorf("Expected %v, got %v.", expected[i], got[i])
        }
    }
}

func CheckHashmaps(t *testing.T, expected map[int]bool, got map[int]bool) {
    for k, v := range expected {
        if got[k] != v {
            t.Errorf("Expected %t for %d", v, k)
        }
    }
    for k, v := range got {
        if expected[k] != v {
            t.Errorf("Expected %t for %d", v, k)
        }
    }
}

const chart = `...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....`

func TestParseGalaxies(t *testing.T) {
    var expected, got []Position
    var gotY, gotX int

    expected = []Position{
        Position { y: 0, x: 3 },
        Position { y: 1, x: 7 },
        Position { y: 2, x: 0 },
        Position { y: 4, x: 6 },
        Position { y: 5, x: 1 },
        Position { y: 6, x: 9 },
        Position { y: 8, x: 7 },
        Position { y: 9, x: 0 },
        Position { y: 9, x: 4 },
    }
    gotY, gotX, got = ParseGalaxies(bufio.NewScanner(strings.NewReader(chart)))
    if gotY != 10 {
        t.Errorf("Expected 10, got %d", gotY)
    }
    if gotX != 10 {
        t.Errorf("Expected 10, got %d", gotX)
    }
    if len(expected) != len(got) {
        t.Error("Different list sizes")
    }
    for i := range expected {
        if expected[i] != got[i] {
            t.Errorf("Expected %+v, got %+v", expected[i], got[i])
        }
    }
}

func TestParseEmpty(t *testing.T) {
    input := []Position{
        Position { y: 0, x: 3 },
        Position { y: 1, x: 7 },
        Position { y: 2, x: 0 },
        Position { y: 4, x: 6 },
        Position { y: 5, x: 1 },
        Position { y: 6, x: 9 },
        Position { y: 8, x: 7 },
        Position { y: 9, x: 0 },
        Position { y: 9, x: 4 },
    }

    var expectedY, gotY map[int]bool
    var expectedX, gotX map[int]bool

    expectedY = map[int]bool {
        0: false,
        1: false,
        2: false,
        3: true,
        4: false,
        5: false,
        6: false,
        7: true,
        8: false,
        9: false,
    }
    expectedX = map[int]bool {
        0: false,
        1: false,
        2: true,
        3: false,
        4: false,
        5: true,
        6: false,
        7: false,
        8: true,
        9: false,
    }
    gotY, gotX = ParseEmpty(10, 10, input)
    CheckHashmaps(t, expectedX, gotX)
    CheckHashmaps(t, expectedY, gotY)
}

func TestGetPairs(t *testing.T) {
    input := []Position{
        Position { y: 0, x: 3 },
        Position { y: 1, x: 7 },
        Position { y: 2, x: 0 },
        Position { y: 4, x: 6 },
        Position { y: 5, x: 1 },
        Position { y: 6, x: 9 },
        Position { y: 8, x: 7 },
        Position { y: 9, x: 0 },
        Position { y: 9, x: 4 },
    }
    var got []Pair

    got = GetPairs(input)
    if len(got) != 36 {
        t.Errorf("Expected 36 pairs, got %d pairs: %v", len(got), got)
    }
}

func TestCalculateDistance(t *testing.T) {
    input := []Position{
        Position { y: 0, x: 3 },
        Position { y: 1, x: 7 },
        Position { y: 2, x: 0 },
        Position { y: 4, x: 6 },
        Position { y: 5, x: 1 },
        Position { y: 6, x: 9 },
        Position { y: 8, x: 7 },
        Position { y: 9, x: 0 },
        Position { y: 9, x: 4 },
    }
    emptyY := map[int]bool {
        0: false,
        1: false,
        2: false,
        3: true,
        4: false,
        5: false,
        6: false,
        7: true,
        8: false,
        9: false,
    }
    emptyX := map[int]bool {
        0: false,
        1: false,
        2: true,
        3: false,
        4: false,
        5: true,
        6: false,
        7: false,
        8: true,
        9: false,
    }
    var expected, got int

    got = CalculateDistance(Pair { first: input[4], second: input[8] }, emptyY, emptyX)
    expected = 9
    if expected != got {
        t.Errorf("Expected %d, got %d.", expected, got)
    }

    got = CalculateDistance(Pair { first: input[0], second: input[6] }, emptyY, emptyX)
    expected = 15
    if expected != got {
        t.Errorf("Expected %d, got %d.", expected, got)
    }

    got = CalculateDistance(Pair { first: input[2], second: input[5] }, emptyY, emptyX)
    expected = 17
    if expected != got {
        t.Errorf("Expected %d, got %d.", expected, got)
    }

    got = CalculateDistance(Pair { first: input[7], second: input[8] }, emptyY, emptyX)
    expected = 5
    if expected != got {
        t.Errorf("Expected %d, got %d.", expected, got)
    }
}
