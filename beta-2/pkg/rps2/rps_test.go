package rps

import (
    "bufio"
    "strings"
    "testing"
)

func TestImplement(t *testing.T) {
    var expected, got Round

    expected = Round{them: Rock, you: Rock}
    got = Strategy{them: Rock, result: Tie}.Implement()
    if got != expected {
        t.Errorf("Expected %#v, got %#v.", expected, got)
    }

    expected = Round{them: Paper, you: Rock}
    got = Strategy{them: Paper, result: Lose}.Implement()
    if got != expected {
        t.Errorf("Expected %#v, got %#v.", expected, got)
    }

    expected = Round{them: Scissors, you: Rock}
    got = Strategy{them: Scissors, result: Win}.Implement()
    if got != expected {
        t.Errorf("Expected %#v, got %#v.", expected, got)
    }
}

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
    expected := []Strategy{
        {them: Rock, result: Tie},
        {them: Paper, result: Lose},
        {them: Scissors, result: Win},
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
