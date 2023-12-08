package maps

import (
    "bufio"
    "regexp"
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

func IsAllEnded(maps []*Map) bool {
    for i, _ := range maps {
        if !strings.HasSuffix(maps[i].name, "Z") {
            return false
        }
    }
    return true
}

func CountGhostMovements(directions []Direction, maps []Map) int {
    var current []*Map = FindGhostStart(maps)
    var gps = Directions(directions)
    var direction Direction
    count := 0
    for !IsAllEnded(current) {
        direction = gps()
        for i, _ := range current {
            current[i] = current[i].FollowDirection(direction)
        }
        count += 1
    }

    return count
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
