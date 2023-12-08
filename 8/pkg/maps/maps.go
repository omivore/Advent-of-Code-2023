package maps

import (
    "bufio"
    "regexp"
    "slices"
    "strings"
)

type Direction int
type Map struct {
    name string
    paths map[Direction]*Map
}
const (
    left Direction = iota
    right
)

func (m Map) FollowDirection(direction Direction) *Map {
    return m.paths[direction]
}

func Directions(directions []Direction) func() Direction {
    i := 0
    return func() (next Direction) {
        if i < len(directions) {
            next = directions[i]
            i += 1
        } else {
            next = directions[0]
            i = 1
        }

        return
    }
}

func FindStart(maps []Map) *Map {
    for _, m := range maps {
        if m.name == "AAA" {
            return &m
        }
    }
    panic("Could not find starting map")
}

func CountMovements(directions []Direction, maps []Map) int {
    var current *Map = FindStart(maps)
    var gps = Directions(directions)
    var direction Direction
    count := 0
    for current.name != "ZZZ" {
        direction = gps()
        current = current.FollowDirection(direction)
        count += 1
    }

    return count
}

func FindGhostStart(maps []Map) (starts []*Map) {
    for i, _ := range maps {
        if strings.HasSuffix(maps[i].name, "A") {
            starts = append(starts, &maps[i])
        }
    }
    return
}

func IsAllSameStep(steps []int) bool {
    if len(steps) == 0 {
        return true
    }
    value := steps[0]
    for _, s := range steps {
        if s != value {
            return false
        }
    }
    return true
}

func IncrementLessers(current []int, loop []int) []int {
    if len(current) != len(loop) {
        panic("Not enough loop steps for each current stepping")
    }

    topValue := slices.Max(current)
    for i, _ := range current {
        for current[i] < topValue {
            current[i] += loop[i]
        }
    }
    return current
}

func CountGhostMovements(directions []Direction, maps []Map) int {
    var current []*Map = FindGhostStart(maps)
    var gpss = make([]func() Direction, len(current))
    var initialSteps = make([]int, len(current))
    var direction Direction
    for i, _ := range current {
        gpss[i] = Directions(directions)
        for !strings.HasSuffix(current[i].name, "Z") {
            direction = gpss[i]()
            current[i] = current[i].FollowDirection(direction)
            initialSteps[i] += 1
        }
    }
    var loopSteps = make([]int, len(current))
    for i, _ := range current {
        direction = gpss[i]()
        current[i] = current[i].FollowDirection(direction)
        loopSteps[i] += 1
        for !strings.HasSuffix(current[i].name, "Z") {
            direction = gpss[i]()
            current[i] = current[i].FollowDirection(direction)
            loopSteps[i] += 1
        }
    }

    currentSteps := initialSteps
    for !IsAllSameStep(currentSteps) {
        currentSteps = IncrementLessers(currentSteps, loopSteps)
    }

    return currentSteps[0]
}

var r = regexp.MustCompile(`^([A-Z1-9]{3}) = \(([A-Z1-9]{3}), ([A-Z1-9]{3})\)$`)

func ParseData(scanner *bufio.Scanner) (directions []Direction, maps []Map) {
    scanned := scanner.Scan()
    if !scanned {
        panic("Could not read directions")
    }
    for _, letter := range strings.Split(scanner.Text(), "") {
        var direction Direction
        switch letter {
        case "L":
            direction = left
        case "R":
            direction = right
        default:
            panic("Unrecognized direction")
        }
        directions = append(directions, direction)
    }
    scanner.Scan()  // Eat empty line

    // First, build all the empty Maps
    nameToMap := make(map[string]*Map)
    saveLines := []string{}
    for scanner.Scan() {
        mapData := scanner.Text()
        saveLines = append(saveLines, mapData)
        matches := r.FindStringSubmatch(mapData)
        if len(matches) != 4 {
            panic("Could not identify map name")
        }
        maps = append(
            maps,
            Map {
                name: matches[1],
                paths: make(map[Direction]*Map),
            },
        )
        nameToMap[matches[1]] = &maps[len(maps) - 1]
    }
    for _, mapData := range saveLines {
        matches := r.FindStringSubmatch(mapData)
        if len(matches) != 4 {
            panic("Could not identify map paths")
        }
        nameToMap[matches[1]].paths[left] = nameToMap[matches[2]]
        nameToMap[matches[1]].paths[right] = nameToMap[matches[3]]
    }

    return
}
