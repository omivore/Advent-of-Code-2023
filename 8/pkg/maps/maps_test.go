package maps

import (
    "bufio"
    "slices"
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

func TestFollowDirection(t *testing.T) {
    a := Map {
        name: "AAA",
        paths: make(map[Direction]*Map),
    }
    b := Map {
        name: "BBB",
        paths: make(map[Direction]*Map),
    }
    c := Map {
        name: "CCC",
        paths: make(map[Direction]*Map),
    }
    d := Map {
        name: "DDD",
        paths: make(map[Direction]*Map),
    }
    e := Map {
        name: "EEE",
        paths: make(map[Direction]*Map),
    }
    g := Map {
        name: "GGG",
        paths: make(map[Direction]*Map),
    }
    z := Map {
        name: "ZZZ",
        paths: make(map[Direction]*Map),
    }
    a.paths[left] = &b
    a.paths[right] = &c
    b.paths[left] = &d
    b.paths[right] = &e
    c.paths[left] = &z
    c.paths[right] = &g
    d.paths[left] = &d
    d.paths[right] = &d
    e.paths[left] = &e
    e.paths[right] = &e
    g.paths[left] = &g
    g.paths[right] = &g
    z.paths[left] = &z
    z.paths[right] = &z

    var got, expected string
    got = a.FollowDirection(right).name
    expected = "CCC"
    if expected != got {
        t.Errorf("Expected %s, got %s.", expected, got)
    }

    got = c.FollowDirection(left).name
    expected = "ZZZ"
    if expected != got {
        t.Errorf("Expected %s, got %s.", expected, got)
    }

    a2 := Map {
        name: "AAA",
        paths: make(map[Direction]*Map),
    }
    b2 := Map {
        name: "BBB",
        paths: make(map[Direction]*Map),
    }
    z2 := Map {
        name: "ZZZ",
        paths: make(map[Direction]*Map),
    }
    a2.paths[left] = &b2
    a2.paths[right] = &b2
    b2.paths[left] = &a2
    b2.paths[right] = &z2
    z2.paths[left] = &z2
    z2.paths[right] = &z2

    got = a2.FollowDirection(left).name
    expected = "BBB"
    if expected != got {
        t.Errorf("Expected %s, got %s.", expected, got)
    }

    got = b2.FollowDirection(left).name
    expected = "AAA"
    if expected != got {
        t.Errorf("Expected %s, got %s.", expected, got)
    }

    got = a2.FollowDirection(right).name
    expected = "BBB"
    if expected != got {
        t.Errorf("Expected %s, got %s.", expected, got)
    }

    got = b2.FollowDirection(right).name
    expected = "ZZZ"
    if expected != got {
        t.Errorf("Expected %s, got %s.", expected, got)
    }
}

func TestCountGhostMovements(t *testing.T) {
    a1 := Map {
        name: "11A",
        paths: make(map[Direction]*Map),
    }
    b1 := Map {
        name: "11B",
        paths: make(map[Direction]*Map),
    }
    z1 := Map {
        name: "11Z",
        paths: make(map[Direction]*Map),
    }
    a2 := Map {
        name: "22A",
        paths: make(map[Direction]*Map),
    }
    b2 := Map {
        name: "22B",
        paths: make(map[Direction]*Map),
    }
    c2 := Map {
        name: "22C",
        paths: make(map[Direction]*Map),
    }
    z2 := Map {
        name: "22Z",
        paths: make(map[Direction]*Map),
    }
    x := Map {
        name: "XXX",
        paths: make(map[Direction]*Map),
    }
    a1.paths[left] = &b1
    a1.paths[right] = &x
    b1.paths[left] = &x
    b1.paths[right] = &z1
    z1.paths[left] = &b1
    z1.paths[right] = &x
    a2.paths[left] = &b2
    a2.paths[right] = &x
    b2.paths[left] = &c2
    b2.paths[right] = &c2
    c2.paths[left] = &z2
    c2.paths[right] = &z2
    z2.paths[left] = &b2
    z2.paths[right] = &b2
    x.paths[left] = &x
    x.paths[right] = &x

    var got, expected int
    got = CountGhostMovements(
        []Direction{left, right},
        []Map{a1, b1, z1, a2, b2, c2, z2, x},
    )
    expected = 6
    if expected != got {
        t.Errorf("Expected %d, got %d.", expected, got)
    }
}

func TestCountMovements(t *testing.T) {
    a := Map {
        name: "AAA",
        paths: make(map[Direction]*Map),
    }
    b := Map {
        name: "BBB",
        paths: make(map[Direction]*Map),
    }
    c := Map {
        name: "CCC",
        paths: make(map[Direction]*Map),
    }
    d := Map {
        name: "DDD",
        paths: make(map[Direction]*Map),
    }
    e := Map {
        name: "EEE",
        paths: make(map[Direction]*Map),
    }
    g := Map {
        name: "GGG",
        paths: make(map[Direction]*Map),
    }
    z := Map {
        name: "ZZZ",
        paths: make(map[Direction]*Map),
    }
    a.paths[left] = &b
    a.paths[right] = &c
    b.paths[left] = &d
    b.paths[right] = &e
    c.paths[left] = &z
    c.paths[right] = &g
    d.paths[left] = &d
    d.paths[right] = &d
    e.paths[left] = &e
    e.paths[right] = &e
    g.paths[left] = &g
    g.paths[right] = &g
    z.paths[left] = &z
    z.paths[right] = &z

    var got, expected int
    got = CountMovements(
        []Direction{right, left},
        []Map{a, b, c, d, e, g, z},
    )
    expected = 2
    if expected != got {
        t.Errorf("Expected %d, got %d.", expected, got)
    }

    a2 := Map {
        name: "AAA",
        paths: make(map[Direction]*Map),
    }
    b2 := Map {
        name: "BBB",
        paths: make(map[Direction]*Map),
    }
    z2 := Map {
        name: "ZZZ",
        paths: make(map[Direction]*Map),
    }
    a2.paths[left] = &b2
    a2.paths[right] = &b2
    b2.paths[left] = &a2
    b2.paths[right] = &z2
    z2.paths[left] = &z2
    z2.paths[right] = &z2

    got = CountMovements(
        []Direction{left, left, right},
        []Map{a2, b2, z2},
    )
    expected = 6
    if expected != got {
        t.Errorf("Expected %d, got %d.", expected, got)
    }
}

func TestParseData(t *testing.T) {
    var got_dirs, expected_dirs []Direction
    var got_maps, expected_maps []Map
    var input string

    input = `RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)`

    expected_dirs = []Direction{right, left}
    expected_maps = make([]Map, 7)
    expected_maps[0] = Map {
        name: "AAA",
        paths: make(map[Direction]*Map),
    }
    expected_maps[1] = Map {
        name: "BBB",
        paths: make(map[Direction]*Map),
    }
    expected_maps[2] = Map {
        name: "CCC",
        paths: make(map[Direction]*Map),
    }
    expected_maps[3] = Map {
        name: "DDD",
        paths: make(map[Direction]*Map),
    }
    expected_maps[4] = Map {
        name: "EEE",
        paths: make(map[Direction]*Map),
    }
    expected_maps[5] = Map {
        name: "GGG",
        paths: make(map[Direction]*Map),
    }
    expected_maps[6] = Map {
        name: "ZZZ",
        paths: make(map[Direction]*Map),
    }
    expected_maps[0].paths[left] = &expected_maps[1]
    expected_maps[0].paths[right] = &expected_maps[2]
    expected_maps[1].paths[left] = &expected_maps[3]
    expected_maps[1].paths[right] = &expected_maps[4]
    expected_maps[2].paths[left] = &expected_maps[6]
    expected_maps[2].paths[right] = &expected_maps[5]
    expected_maps[3].paths[left] = &expected_maps[3]
    expected_maps[3].paths[right] = &expected_maps[3]
    expected_maps[4].paths[left] = &expected_maps[4]
    expected_maps[4].paths[right] = &expected_maps[4]
    expected_maps[5].paths[left] = &expected_maps[5]
    expected_maps[5].paths[right] = &expected_maps[5]
    expected_maps[6].paths[left] = &expected_maps[6]
    expected_maps[6].paths[right] = &expected_maps[6]
    got_dirs, got_maps = ParseData(bufio.NewScanner(strings.NewReader(input)))
    if len(got_dirs) != len(expected_dirs) {
        t.Errorf("Expected %d elements, got %d elements.", len(expected_dirs), len(got_dirs))
    }
    if slices.Compare(got_dirs, expected_dirs) != 0 {
        t.Errorf("Expected %v, got %v.", expected_dirs, got_dirs)
    }
    if len(got_maps) != len(expected_maps) {
        t.Errorf("Expected %d elements, got %d elements.", len(expected_maps), len(got_maps))
    }
    for i := range expected_maps {
        if got_maps[i].name != expected_maps[i].name {
            t.Errorf("Expected %s, got %s.", expected_maps[i].name, got_maps[i].name)
        }
        if got_maps[i].paths[left] == expected_maps[i].paths[left] {
            t.Errorf("Expected %p, got %p.", expected_maps[i].paths[left], got_maps[i].paths[right])
        }
        if got_maps[i].paths[right] == expected_maps[i].paths[right] {
            t.Errorf("Expected %p, got %p.", expected_maps[i].paths[right], got_maps[i].paths[right])
        }
    }

    input = `LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)`

    expected_dirs = []Direction{left, left, right}
    expected_maps = make([]Map, 3)
    expected_maps[0] = Map {
        name: "AAA",
        paths: make(map[Direction]*Map),
    }
    expected_maps[1] = Map {
        name: "BBB",
        paths: make(map[Direction]*Map),
    }
    expected_maps[2] = Map {
        name: "ZZZ",
        paths: make(map[Direction]*Map),
    }
    expected_maps[0].paths[left] = &expected_maps[1]
    expected_maps[0].paths[right] = &expected_maps[1]
    expected_maps[1].paths[left] = &expected_maps[0]
    expected_maps[1].paths[right] = &expected_maps[2]
    expected_maps[2].paths[left] = &expected_maps[2]
    expected_maps[2].paths[right] = &expected_maps[2]
    got_dirs, got_maps = ParseData(bufio.NewScanner(strings.NewReader(input)))
    if len(got_dirs) != len(expected_dirs) {
        t.Errorf("Expected %d elements, got %d elements.", len(expected_dirs), len(got_dirs))
    }
    if slices.Compare(got_dirs, expected_dirs) != 0 {
        t.Errorf("Expected %v, got %v.", expected_dirs, got_dirs)
    }
    if len(got_maps) != len(expected_maps) {
        t.Errorf("Expected %d elements, got %d elements.", len(expected_maps), len(got_maps))
    }
    for i := range expected_maps {
        if got_maps[i].name != expected_maps[i].name {
            t.Errorf("Expected %s, got %s.", expected_maps[i].name, got_maps[i].name)
        }
        if got_maps[i].paths[left] == expected_maps[i].paths[left] {
            t.Errorf("Expected %p, got %p.", expected_maps[i].paths[left], got_maps[i].paths[right])
        }
        if got_maps[i].paths[right] == expected_maps[i].paths[right] {
            t.Errorf("Expected %p, got %p.", expected_maps[i].paths[right], got_maps[i].paths[right])
        }
    }
}
