package rps

import (
    "bufio"
    "strings"
    "testing"
)

func TestPoints(t *testing.T) {
    var got int

    got = Round{you: Paper, them: Rock}.Points()
    if got != 8 {
        t.Errorf("Expected 8, got %d.", got)
    }

    got = Round{you: Rock, them: Paper}.Points()
    if got != 1 {
        t.Errorf("Expected 1, got %d.", got)
    }

    got = Round{you: Scissors, them: Scissors}.Points()
    if got != 6 {
        t.Errorf("Expected 6, got %d.", got)
    }
}

func TestParseData(t *testing.T) {
    input := "A Y\nB X\nC Z\n"
    scanner := bufio.NewScanner(strings.NewReader(input))
    got := ParseData(scanner)
    expected := []Round{
        {them: Rock, you: Paper},
        {them: Paper, you: Rock},
        {them: Scissors, you: Scissors},
    }

    if len(got) != 3 {
        t.Errorf("Expected 3 Rounds, got %d Rounds.", len(got))
    }

    for i := range got {
        if got[i] != expected[i] {
            t.Errorf("Expected %#v, got %#v.", expected[i], got[i])
        }
    }
}
