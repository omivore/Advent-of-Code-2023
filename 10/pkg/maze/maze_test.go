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

const in1 = `-L|F7
7S-7|
L|7||
-L-J|
L|-JF`


const in2 = `7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ`

const in3 = `..........
.S------7.
.|F----7|.
.||....||.
.||....||.
.|L-7F-J|.
.|..||..|.
.L--JL--J.
..........`

const expanded3 = `. . . . . . . . . . 
                    
. S-------------7 . 
  |             |   
. | F---------7 | . 
  | |         | |   
. | | . . . . | | . 
  | |         | |   
. | | . . . . | | . 
  | |         | |   
. | L---7 F---J | . 
  |     | |     |   
. | . . | | . . | . 
  |     | |     |   
. L-----J L-----J . 
                    
. . . . . . . . . . 
                    `

func TestNewMaze(t *testing.T) {
    var expected, got Maze

    expected = Maze {
        area: [][]string{
            []string{"-", "L", "|", "F", "7"},
            []string{"7", "S", "-", "7", "|"},
            []string{"L", "|", "7", "|", "|"},
            []string{"-", "L", "-", "J", "|"},
            []string{"L", "|", "-", "J", "F"},
        },
    }
    got = NewMaze(bufio.NewScanner(strings.NewReader(in1)))
    if len(expected.area) != len(got.area) ||
       len(expected.area[0]) != len(got.area[0]) {
        t.Error("Different map sizes")
    }
    for y, _ := range expected.area {
        for x, _ := range expected.area[y] {
            if expected.area[y][x] != got.area[y][x] {
                t.Errorf(
                    "Expected %s, got %s at (%d, %d)",
                    expected.area[y][x],
                    got.area[y][x],
                    y,
                    x,
                )
            }
        }
    }

    expected = Maze {
        area: [][]string{
            []string{"7", "-", "F", "7", "-"},
            []string{".", "F", "J", "|", "7"},
            []string{"S", "J", "L", "L", "7"},
            []string{"|", "F", "-", "-", "J"},
            []string{"L", "J", ".", "L", "J"},
        },
    }
    got = NewMaze(bufio.NewScanner(strings.NewReader(in2)))
    if len(expected.area) != len(got.area) ||
       len(expected.area[0]) != len(got.area[0]) {
        t.Error("Different map sizes")
    }
    for y, _ := range expected.area {
        for x, _ := range expected.area[y] {
            if expected.area[y][x] != got.area[y][x] {
                t.Errorf("Conflicting token found at (%d, %d)", y, x)
            }
        }
    }
}

func TestFindStart(t *testing.T) {
    var expected, got Position

    got = NewMaze(bufio.NewScanner(strings.NewReader(in1))).FindStart()
    expected = Position { y: 1, x: 1 }
    if expected != got {
        t.Errorf("Expected %v, got %v.", expected, got)
    }

    got = NewMaze(bufio.NewScanner(strings.NewReader(in2))).FindStart()
    expected = Position { y: 2, x: 0 }
    if expected != got {
        t.Errorf("Expected %v, got %v.", expected, got)
    }
}

func TestGetDirections(t *testing.T) {
    var expected, got []Direction

    got = NewMaze(bufio.NewScanner(strings.NewReader(in1))).GetDirections(Position {y: 0, x: 0})
    expected = []Direction{}
    CheckArray(t, expected, got)

    got = NewMaze(bufio.NewScanner(strings.NewReader(in1))).GetDirections(Position {y: 1, x: 1})
    expected = []Direction{east, south}
    CheckArray(t, expected, got)

    got = NewMaze(bufio.NewScanner(strings.NewReader(in1))).GetDirections(Position {y: 1, x: 2})
    expected = []Direction{west, east}
    CheckArray(t, expected, got)

    got = NewMaze(bufio.NewScanner(strings.NewReader(in1))).GetDirections(Position {y: 1, x: 3})
    expected = []Direction{west, south}
    CheckArray(t, expected, got)

    got = NewMaze(bufio.NewScanner(strings.NewReader(in1))).GetDirections(Position {y: 2, x: 3})
    expected = []Direction{north, south}
    CheckArray(t, expected, got)

    got = NewMaze(bufio.NewScanner(strings.NewReader(in1))).GetDirections(Position {y: 3, x: 3})
    expected = []Direction{north, west}
    CheckArray(t, expected, got)

    got = NewMaze(bufio.NewScanner(strings.NewReader(in1))).GetDirections(Position {y: 3, x: 2})
    expected = []Direction{west, east}
    CheckArray(t, expected, got)

    got = NewMaze(bufio.NewScanner(strings.NewReader(in1))).GetDirections(Position {y: 3, x: 1})
    expected = []Direction{north, east}
    CheckArray(t, expected, got)

    got = NewMaze(bufio.NewScanner(strings.NewReader(in1))).GetDirections(Position {y: 2, x: 1})
    expected = []Direction{north, south}
    CheckArray(t, expected, got)

    got = NewMaze(bufio.NewScanner(strings.NewReader(in1))).GetDirections(Position {y: 1, x: 0})
    expected = []Direction{south}
    CheckArray(t, expected, got)
}

func TestFindFarthest(t *testing.T) {
    var expected, got int

    got = NewMaze(bufio.NewScanner(strings.NewReader(in1))).FindFarthest()
    expected = 4
    if expected != got {
        t.Errorf("Expected %d, got %d.", expected, got)
    }

    got = NewMaze(bufio.NewScanner(strings.NewReader(in2))).FindFarthest()
    expected = 8
    if expected != got {
        t.Errorf("Expected %d, got %d.", expected, got)
    }
}

func TestExpandMaze(t *testing.T) {
    var expected, got Maze

    got = NewMaze(bufio.NewScanner(strings.NewReader(in3))).ExpandMaze()
    expected = NewMaze(bufio.NewScanner(strings.NewReader(expanded3)))
    if len(expected.area) != len(got.area) ||
       len(expected.area[0]) != len(got.area[0]) {
        t.Error("Different map sizes")
    }
    for y, _ := range expected.area {
        for x, _ := range expected.area[y] {
            if expected.area[y][x] != got.area[y][x] {
                t.Errorf(
                    "Expected %s, got %s at (%d, %d)",
                    expected.area[y][x],
                    got.area[y][x],
                    y,
                    x,
                )
            }
        }
    }
}

func TestCountInterior(t *testing.T) {
    var expected, got int

    got = NewMaze(bufio.NewScanner(strings.NewReader(in3))).ExpandMaze().CountInterior()
    expected = 4
    if got != expected {
        t.Errorf("Expected %d, got %d", expected, got)
    }
}
