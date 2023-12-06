package boat

import (
    "bufio"
    "strings"
    "testing"
)

func TestChargeToBeat(t *testing.T) {
    var got, expected []int

    got = Race{time: 7, distance: 9}.HoldToBeat()
    expected = []int{2, 3, 4, 5}

    if len(got) != len(expected) {
        t.Errorf("Expected %d elements, got %d elements.", len(expected), len(got))
    }
    for i := range expected {
        if got[i] != expected[i] {
            t.Errorf("Expected [2, 3, 4, 5], got %v.", got)
        }
    }

    got = Race{time: 15, distance: 40}.HoldToBeat()
    expected = []int{4, 5, 6, 7, 8, 9, 10, 11}

    if len(got) != len(expected) {
        t.Errorf("Expected %d elements, got %d elements.", len(expected), len(got))
    }
    for i := range expected {
        if got[i] != expected[i] {
            t.Errorf("Expected [2, 3, 4, 5], got %v.", got)
        }
    }


    got = Race{time: 30, distance: 200}.HoldToBeat()
    expected = []int{11, 12, 13, 14, 15, 16, 17, 18, 19}

    if len(got) != len(expected) {
        t.Errorf("Expected %d elements, got %d elements.", len(expected), len(got))
    }
    for i := range expected {
        if got[i] != expected[i] {
            t.Errorf("Expected [2, 3, 4, 5], got %v.", got)
        }
    }

}

func TestParseData(t *testing.T) {
    input := "Time:      7  15   30\nDistance:  9  40  200"
    var scanner *bufio.Scanner
    var got, expected []Race

    scanner = bufio.NewScanner(strings.NewReader(input))
    got = ParseData(scanner, false)
    expected = []Race{
        {time: 7, distance: 9},
        {time: 15, distance: 40},
        {time: 30, distance: 200},
    }

    if len(got) != 3 {
        t.Errorf("Expected 3 Rounds, got %d Rounds.", len(got))
    }

    for i := range got {
        if got[i] != expected[i] {
            t.Errorf("Expected %#v, got %#v.", expected[i], got[i])
        }
    }

    scanner = bufio.NewScanner(strings.NewReader(input))
    got = ParseData(scanner, true)
    expected = []Race{
        {time: 71530, distance: 940200},
    }

    if len(got) != 1 {
        t.Errorf("Expected 1 Rounds, got %d Rounds.", len(got))
    }

    for i := range got {
        if got[i] != expected[i] {
            t.Errorf("Expected %#v, got %#v.", expected[i], got[i])
        }
    }
}
