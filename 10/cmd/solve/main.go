package main

import (
    "bufio"
    "fmt"
    "os"

    "flag"

    pkg "github.com/omivore/Advent-of-Code-2023/10/pkg/maze"
)

func check(e error) {
    if e != nil {
        panic(e)
    }
}

func main() {
    interiors := flag.Bool(
        "interiors",
        false,
        "Count the interior tiles",
    )
    flag.Parse()
    if flag.NArg() < 1 {
        panic("No command line arguments found")
    }

    file, err := os.Open(flag.Arg(0))
    check(err)
    defer file.Close()

    scanner := bufio.NewScanner(file)
    maze := pkg.NewMaze(scanner)
    var result int
    if *interiors {
        result = maze.ExpandMaze().CountInterior()
    } else {
        result = maze.FindFarthest()
    }
    fmt.Printf("%d\n", result)
}
