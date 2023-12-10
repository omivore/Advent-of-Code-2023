package maps

import (
    "bufio"
    "strings"
)

type Direction int
const (
    north Direction = iota
    west
    east
    south
)

type Position struct {
    y int
    x int
}

type Maze struct {
    area [][]string
}

func NewMaze(scanner *bufio.Scanner) Maze {
    area := [][]string{}
    for scanner.Scan() {
        line := scanner.Text()
        area = append(area, strings.Split(line, ""))
    }

    return Maze {
        area: area,
    }
}

func (m Maze) FindStart() Position {
    for y, _ := range m.area {
        for x, _ := range m.area[y] {
            if m.area[y][x] == "S" {
                return Position {
                    y: y,
                    x: x,
                }
            }
        }
    }
   panic("Could not find starting position")
}

func (m Maze) GetDirections(pos Position) []Direction {
    var options []Direction
    switch m.Get(pos) {
    case "S":
        options = []Direction{north, west, east, south}
    case "|":
        options = []Direction{north, south}
    case "-":
        options = []Direction{west, east}
    case "L":
        options = []Direction{north, east}
    case "J":
        options = []Direction{north, west}
    case "7":
        options = []Direction{west, south}
    case "F":
        options = []Direction{east, south}
    case ".":
        options = []Direction{}
    }

    var connected []Direction
    for _, option := range options {
        child := m.Move(pos, option)
        if child == pos {
            continue
        }
        switch m.Get(child) {
        case "S":
            connected = append(connected, option)
        case "|":
            if option == north || option == south {
                connected = append(connected, option)
            }
        case "-":
            if option == east || option == west {
                connected = append(connected, option)
            }
        case "L":
            if option == south || option == west {
                connected = append(connected, option)
            }
        case "J":
            if option == south || option == east {
                connected = append(connected, option)
            }
        case "7":
            if option == north || option == east {
                connected = append(connected, option)
            }
        case "F":
            if option == north || option == west {
                connected = append(connected, option)
            }
        case ".":
            continue
        }
    }

    return connected
}

func (m Maze) Get(pos Position) string {
    return m.area[pos.y][pos.x]
}

func (m Maze) Move(pos Position, direction Direction) Position {
    new_y, new_x := pos.y, pos.x
    switch direction {
    case north:
        new_y -= 1
    case east:
        new_x += 1
    case west:
        new_x -= 1
    case south:
        new_y += 1
    }

    max_y, max_x := len(m.area) - 1, len(m.area[0]) - 1
    new_y = min(max_y, max(0, new_y))
    new_x = min(max_x, max(0, new_x))

    return Position {
        y: new_y,
        x: new_x,
    }
}

type Ranking struct {
    pos Position
    dist int
}

func (m Maze) IdentifyLoop() (visited []Ranking) {
    var current Ranking
    visiting := []Ranking{ Ranking { pos: m.FindStart(), dist: 0 } }

    for len(visiting) > 0 {
        current, visiting = visiting[0], visiting[1:]
        visited = append(visited, current)
        opts := m.GetDirections(current.pos)
        DirectionsLoop:
        for _, opt := range opts {
            child := m.Move(current.pos, opt)
            for _, past := range visited {
                if past.pos == child {
                    continue DirectionsLoop
                }
            }
            for _, past := range visiting {
                if past.pos == child {
                    continue DirectionsLoop
                }
            }
            visiting = append(visiting, Ranking { pos: child, dist: current.dist + 1 })
        }
    }

    return
}

func (m Maze) FindFarthest() int {
    var farthest Ranking
    for _, point := range m.IdentifyLoop() {
        if point.dist > farthest.dist {
            farthest = point
        }
    }
    return farthest.dist
}

func (m Maze) ExpandMaze() Maze {
    loops := []Position{}
    for _, rank := range m.IdentifyLoop() {
        loops = append(loops, rank.pos)
    }

    bigArea := make([][]string, len(m.area) * 2)
    for i := range bigArea {
        bigArea[i] = make([]string, len(m.area[i / 2]) * 2)
    }

    for y := range m.area {
        for x := range m.area[y] {
            here := Position { y: y, x: x }
            var there Position
            eastBridge := " "
            southBridge := " "
            self := "."
            // Build east?
            there = m.Move(here, east)
            if here != there {
                switch m.Get(here) {
                case "S", "-", "L", "F":
                    switch m.Get(there) {
                    case "S", "-", "J", "7":
                        eastBridge = "-"
                    }
                }
            }
            // Build south?
            there = m.Move(here, south)
            if here != there {
                switch m.Get(here) {
                case "S", "|", "7", "F":
                    switch m.Get(there) {
                    case "S", "|", "L", "J":
                        southBridge = "|"
                    }
                }
            }
            // Am part of loop?
            for _, loop := range loops {
                if here == loop {
                    self = m.Get(here)
                }
            }

            bigArea[2 * y][2 * x] = self
            bigArea[2 * y + 1][2 * x] = southBridge
            bigArea[2 * y][2 * x + 1] = eastBridge
            bigArea[2 * y + 1][2 * x + 1] = " "
        }
    }

    return Maze {
        area: bigArea,
    }
}

const (
    inside = iota
    outside
)

func (m Maze) CountInterior() int {
    interior := 0
    visited := []Position{}
    for y := range m.area {
        CellSearch:
        for x := range m.area[y] {
            cell := Position { y: y, x: x }
            if m.Get(cell) == "." || m.Get(cell) == " " {
                for _, past := range visited {
                    if past == cell {
                        continue CellSearch
                    }
                }

                newVisited, isInside := m.FloodAndPaint(cell)
                if isInside {
                    for _, newVisit := range newVisited {
                        if m.Get(newVisit) == "." {
                            interior += 1
                        }
                    }
                }
                visited = append(visited, newVisited...)
            }
        }
    }

    return interior
}

func (m Maze) FloodAndPaint(start Position) (visited []Position, isInside bool) {
    isInside = true

    var current Position
    visiting := []Position{ start }
    for len(visiting) > 0 {
        current, visiting = visiting[0], visiting[1:]
        visited = append(visited, current)

        DirectionsLoop:
        for _, dir := range []Direction{north, east, south, west} {
            child := m.Move(current, dir)
            if current == child {
                isInside = false
                continue
            }
            if m.Get(child) == " " || m.Get(child) == "." {
                for _, past := range visited {
                    if past == child {
                        continue DirectionsLoop
                    }
                }
                for _, past := range visiting {
                    if past == child {
                        continue DirectionsLoop
                    }
                }
                visiting = append(visiting, child)
            }
        }
    }

    return
}
