package main

import (
    "bufio"
    "fmt"
    "os"

    "flag"

    "github.com/omivore/Advent-of-Code-2023/7/pkg/poker"
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
    bids := poker.ParseData(scanner)

    result := poker.TotalWinnings(bids)
    fmt.Printf("%d\n", result)
}
