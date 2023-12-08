package main

import (
    "bufio"
    "fmt"
    "os"

    "flag"

    pkg "github.com/omivore/Advent-of-Code-2023/8/pkg/maps"
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
    directions, maps := pkg.ParseData(scanner)
    result := pkg.CountMovements(directions, maps)
    fmt.Printf("%d\n", result)
}
