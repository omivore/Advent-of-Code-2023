package poker

import (
    "bufio"
    "slices"
    "strings"
    "testing"
)

func TestCalculateValue(t *testing.T) {
    var got, expected []Value

    got = CalculateValue([]Value{c3, c2, cT, c3, cK})
    expected = []Value{h1p, c3, c2, cT, c3, cK}
    if len(got) != len(expected) {
        t.Errorf("Expected %d elements, got %d elements.", len(expected), len(got))
    }
    for i := range expected {
        if got[i] != expected[i] {
            t.Errorf("Expected %d, got %d.", expected[i], got[i])
        }
    }

    got = CalculateValue([]Value{cT, c5, c5, cJ, c5})
    expected = []Value{h3k, cT, c5, c5, cJ, c5}
    if len(got) != len(expected) {
        t.Errorf("Expected %d elements, got %d elements.", len(expected), len(got))
    }
    for i := range expected {
        if got[i] != expected[i] {
            t.Errorf("Expected %d, got %d.", expected[i], got[i])
        }
    }

    got = CalculateValue([]Value{cK, cK, c6, c7, c7})
    expected = []Value{h2p, cK, cK, c6, c7, c7}
    if len(got) != len(expected) {
        t.Errorf("Expected %d elements, got %d elements.", len(expected), len(got))
    }
    for i := range expected {
        if got[i] != expected[i] {
            t.Errorf("Expected %d, got %d.", expected[i], got[i])
        }
    }

    got = CalculateValue([]Value{cK, cT, cJ, cJ, cT})
    expected = []Value{h2p, cK, cT, cJ, cJ, cT}
    if len(got) != len(expected) {
        t.Errorf("Expected %d elements, got %d elements.", len(expected), len(got))
    }
    for i := range expected {
        if got[i] != expected[i] {
            t.Errorf("Expected %d, got %d.", expected[i], got[i])
        }
    }

    got = CalculateValue([]Value{cQ, cQ, cQ, cJ, cA})
    expected = []Value{h3k, cQ, cQ, cQ, cJ, cA}
    if len(got) != len(expected) {
        t.Errorf("Expected %d elements, got %d elements.", len(expected), len(got))
    }
    for i := range expected {
        if got[i] != expected[i] {
            t.Errorf("Expected %d, got %d.", expected[i], got[i])
        }
    }



    got = CalculateValue([]Value{cJ, c2, cJ, cJ, cJ})
    expected = []Value{h4k, cJ, c2, cJ, cJ, cJ}
    if len(got) != len(expected) {
        t.Errorf("Expected %d elements, got %d elements.", len(expected), len(got))
    }
    for i := range expected {
        if got[i] != expected[i] {
            t.Errorf("Expected %d, got %d.", expected[i], got[i])
        }
    }

    got = CalculateValue([]Value{cA, cA, cA, cA, cA})
    expected = []Value{h5k, cA, cA, cA, cA, cA}
    if len(got) != len(expected) {
        t.Errorf("Expected %d elements, got %d elements.", len(expected), len(got))
    }
    for i := range expected {
        if got[i] != expected[i] {
            t.Errorf("Expected %d, got %d.", expected[i], got[i])
        }
    }

    got = CalculateValue([]Value{c3, cQ, cQ, c3, cQ})
    expected = []Value{hFh, c3, cQ, cQ, c3, cQ}
    if len(got) != len(expected) {
        t.Errorf("Expected %d elements, got %d elements.", len(expected), len(got))
    }
    for i := range expected {
        if got[i] != expected[i] {
            t.Errorf("Expected %d at %d, got %d.", expected[i], i, got[i])
        }
    }

    got = CalculateValue([]Value{c4, c5, c7, c8, c9})
    expected = []Value{hHi, c4, c5, c7, c8, c9}
    if len(got) != len(expected) {
        t.Errorf("Expected %d elements, got %d elements.", len(expected), len(got))
    }
    for i := range expected {
        if got[i] != expected[i] {
            t.Errorf("Expected %d, got %d.", expected[i], got[i])
        }
    }
}

func TestParseData(t *testing.T) {
    var got, expected []Bid

    input := `32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483`

    expected = []Bid{
        Bid { hand: []Value{c3, c2, cT, c3, cK}, wager: 765},
        Bid { hand: []Value{cT, c5, c5, cJ, c5}, wager: 684},
        Bid { hand: []Value{cK, cK, c6, c7, c7}, wager: 28},
        Bid { hand: []Value{cK, cT, cJ, cJ, cT}, wager: 220},
        Bid { hand: []Value{cQ, cQ, cQ, cJ, cA}, wager: 483},
    }
    got = ParseData(bufio.NewScanner(strings.NewReader(input)))
    if len(got) != len(expected) {
        t.Errorf("Expected %d elements, got %d elements.", len(expected), len(got))
    }
    for i := range expected {
        if got[i].wager != expected[i].wager {
            t.Errorf("Expected %d, got %d.", expected[i], got[i])
        }
        if slices.Compare(got[i].hand, expected[i].hand) != 0 {
            t.Errorf("Expected %v, got %v.", expected[i].hand, got[i].hand)
        }
    }
}

func TestTotalWinnings(t *testing.T) {
    var got, expected int

    got = TotalWinnings([]Bid{
        Bid { hand: []Value{c3, c2, cT, c3, cK}, wager: 765},
        Bid { hand: []Value{cT, c5, c5, cJ, c5}, wager: 684},
        Bid { hand: []Value{cK, cK, c6, c7, c7}, wager: 28},
        Bid { hand: []Value{cK, cT, cJ, cJ, cT}, wager: 220},
        Bid { hand: []Value{cQ, cQ, cQ, cJ, cA}, wager: 483},
    })
    expected = 6440

    if got != expected {
        t.Errorf("Expected %d, got %d.", expected, got)
    }
}
