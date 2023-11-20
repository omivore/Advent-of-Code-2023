package main

import (
    "bufio"
    "fmt"
    "os"

    "flag"

    "github.com/omivore/Advent-of-Code-2023/beta-2/pkg/rps2"
)

func check(e error) {
    if e != nil {
        panic(e)
    }
}

func main() {
    flag.Parse()
    if flag.NArg() < 1 {
        panic("No command line arguments found")
    }

    file, err := os.Open(flag.Arg(0))
    check(err)
    defer file.Close()

    scanner := bufio.NewScanner(file)
    strats := rps.ParseData(scanner)

    sum := 0
    for _, strat := range strats {
        round := strat.Implement()
        sum += round.Points()
    }
    fmt.Printf("Total Points: %d.\n", sum)
}
