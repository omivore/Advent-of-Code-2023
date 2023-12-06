package main

import (
    "bufio"
    "fmt"
    "os"

    "flag"

    "github.com/omivore/Advent-of-Code-2023/6/pkg/boat"
)

func check(e error) {
    if e != nil {
        panic(e)
    }
}

func main() {
    kerning := flag.Bool(
        "kerning",
        false,
        "Fix kerning and treat as one race.",
    )
    flag.Parse()
    if flag.NArg() < 1 {
        panic("No command line arguments found")
    }

    file, err := os.Open(flag.Arg(0))
    check(err)
    defer file.Close()

    scanner := bufio.NewScanner(file)
    races := boat.ParseData(scanner, *kerning)

    product := 1
    for _, race := range races {
        product *= len(race.HoldToBeat())
    }
    fmt.Printf("%d\n", product)
}
