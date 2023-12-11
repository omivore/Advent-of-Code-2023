package main

import (
    "bufio"
    "fmt"
    "os"

    "flag"

    pkg "github.com/omivore/Advent-of-Code-2023/11/pkg/galaxies"
)

func check(e error) {
    if e != nil {
        panic(e)
    }
}

func main() {
    older := flag.Bool(
        "older",
        false,
        "Treat galaxies even older",
    )
    flag.Parse()
    if flag.NArg() < 1 {
        panic("No command line arguments found")
    }

    file, err := os.Open(flag.Arg(0))
    check(err)
    defer file.Close()

    scanner := bufio.NewScanner(file)
    dimY, dimX, gals := pkg.ParseGalaxies(scanner)
    emptyY, emptyX := pkg.ParseEmpty(dimY, dimX, gals)
    pairs := pkg.GetPairs(gals)
    var emptyExpansion, result int
    if *older {
        emptyExpansion = 1000000
    } else {
        emptyExpansion = 2
    }
    for _, pair := range pairs {
        result += pkg.CalculateDistance(pair, emptyY, emptyX, emptyExpansion)
    }
    fmt.Printf("%d\n", result)
}
