package maps

import (
    "bufio"
)

type Position struct {
    y int
    x int
}

type Pair struct {
    first Position
    second Position
}

func ParseGalaxies(scanner *bufio.Scanner) (dimY int, dimX int, gals []Position) {
    for y := 0; scanner.Scan(); y++ {
        line := scanner.Text()
        dimY += 1
        dimX = len(line)
        for x, char := range line {
            if char == '#' {
                gals = append(gals, Position { y: y, x: x })
            }
        }
    }

    return
}

func ParseEmpty(maxY int, maxX int, gals []Position) (map[int]bool, map[int]bool) {
    occupiedX, occupiedY := map[int]bool{}, map[int]bool{}
    for _, gal := range gals {
        occupiedX[gal.x] = true
        occupiedY[gal.y] = true
    }
    emptyY, emptyX := map[int]bool{}, map[int]bool{}
    for y := 0; y < maxY; y++ {
        emptyY[y] = !occupiedY[y]
    }
    for x := 0; x < maxX; x++ {
        emptyX[x] = !occupiedX[x]
    }

    return emptyY, emptyX
}

func GetPairs(gals []Position) (pairs []Pair) {
    for first := 0; first < len(gals) - 1; first++ {
        for second := first + 1; second < len(gals); second++ {
            pairs = append(
                pairs,
                Pair { first: gals[first], second: gals[second] },
            )
        }
    }

    return
}

func CalculateDistance(pair Pair, emptyY map[int]bool, emptyX map[int]bool) (dist int) {
    lessX := min(pair.first.x, pair.second.x)
    moreX := max(pair.first.x, pair.second.x)
    lessY := min(pair.first.y, pair.second.y)
    moreY := max(pair.first.y, pair.second.y)

    for i := lessX; i < moreX; i++ {
        if emptyX[i] {
            dist += 2
        } else {
            dist += 1
        }
    }
    for i := lessY; i < moreY; i++ {
        if emptyY[i] {
            dist += 2
        } else {
            dist += 1
        }
    }

    return
}
