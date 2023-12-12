package main

import (
    "bufio"
    "fmt"
    "os"

    "flag"

    pkg "github.com/omivore/Advent-of-Code-2023/12/pkg/spring"
)

func check(e error) {
    if e != nil {
        panic(e)
    }
}

func main() {
    part2 := flag.Bool(
        "part2",
        false,
        "Activate whatever part 2 wants",
    )
    flag.Parse()
    if flag.NArg() < 1 {
        panic("No command line arguments found")
    }

    file, err := os.Open(flag.Arg(0))
    check(err)
    defer file.Close()

    scanner := bufio.NewScanner(file)

    var result int
    for scanner.Scan() {
        line := scanner.Text()
        var groups []string
        var sets []int
        if *part2 {
            groups, sets = pkg.UnfoldRow(line)
        } else {
            groups, sets = pkg.ParseRow(line)
        }
        numValid := pkg.EnumerateValid(groups, sets)
        fmt.Println(numValid)
        result += numValid
    }
    fmt.Printf("%d\n", result)
}
