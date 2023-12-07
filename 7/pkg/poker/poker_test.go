package poker

import (
    "bufio"
    "slices"
    "strings"
    "testing"
)

func CheckArray[S ~[]E, E comparable](t *testing.T, expected S, got S) {
    if len(got) != len(expected) {
        t.Errorf("Expected %d elements, got %d elements.", len(expected), len(got))
    }
    for i := range expected {
        if got[i] != expected[i] {
            t.Errorf("Expected %v, got %v.", expected[i], got[i])
        }
    }
}

func TestCalculateValue(t *testing.T) {
    var got, expected []Value

    // Test base
    got = CalculateValue([]Value{c3, c2, cT, c3, cK})
    expected = []Value{h1p, c3, c2, cT, c3, cK}
    CheckArray(t, expected, got)

    got = CalculateValue([]Value{cT, c5, c5, cJ, c5})
    expected = []Value{h3k, cT, c5, c5, cJ, c5}
    CheckArray(t, expected, got)

    got = CalculateValue([]Value{cK, cK, c6, c7, c7})
    expected = []Value{h2p, cK, cK, c6, c7, c7}
    CheckArray(t, expected, got)

    got = CalculateValue([]Value{cK, cT, cJ, cJ, cT})
    expected = []Value{h2p, cK, cT, cJ, cJ, cT}
    CheckArray(t, expected, got)

    got = CalculateValue([]Value{cQ, cQ, cQ, cJ, cA})
    expected = []Value{h3k, cQ, cQ, cQ, cJ, cA}
    CheckArray(t, expected, got)


    // Test with joker
    got = CalculateValue([]Value{c3, c2, cT, c3, cK})
    expected = []Value{h1p, c3, c2, cT, c3, cK}
    CheckArray(t, expected, got)

    got = CalculateValue([]Value{cT, c5, c5, joker, c5})
    expected = []Value{h4k, cT, c5, c5, joker, c5}
    CheckArray(t, expected, got)

    got = CalculateValue([]Value{cK, cK, c6, c7, c7})
    expected = []Value{h2p, cK, cK, c6, c7, c7}
    CheckArray(t, expected, got)

    got = CalculateValue([]Value{cK, cT, joker, joker, cT})
    expected = []Value{h4k, cK, cT, joker, joker, cT}
    CheckArray(t, expected, got)

    got = CalculateValue([]Value{cQ, cQ, cQ, joker, cA})
    expected = []Value{h4k, cQ, cQ, cQ, joker, cA}
    CheckArray(t, expected, got)


    // Test other hand types
    got = CalculateValue([]Value{cJ, c2, cJ, cJ, cJ})
    expected = []Value{h4k, cJ, c2, cJ, cJ, cJ}
    CheckArray(t, expected, got)

    got = CalculateValue([]Value{cJ, joker, cJ, cJ, cJ})
    expected = []Value{h5k, cJ, joker, cJ, cJ, cJ}
    CheckArray(t, expected, got)

    got = CalculateValue([]Value{cA, cA, cA, cA, cA})
    expected = []Value{h5k, cA, cA, cA, cA, cA}
    CheckArray(t, expected, got)

    got = CalculateValue([]Value{c3, cQ, cQ, c3, cQ})
    expected = []Value{hFh, c3, cQ, cQ, c3, cQ}
    CheckArray(t, expected, got)

    got = CalculateValue([]Value{c4, c5, c7, c8, c9})
    expected = []Value{hHi, c4, c5, c7, c8, c9}
    CheckArray(t, expected, got)
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
    got = ParseData(bufio.NewScanner(strings.NewReader(input)), false)
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

    expected = []Bid{
        Bid { hand: []Value{c3, c2, cT, c3, cK}, wager: 765},
        Bid { hand: []Value{cT, c5, c5, joker, c5}, wager: 684},
        Bid { hand: []Value{cK, cK, c6, c7, c7}, wager: 28},
        Bid { hand: []Value{cK, cT, joker, joker, cT}, wager: 220},
        Bid { hand: []Value{cQ, cQ, cQ, joker, cA}, wager: 483},
    }
    got = ParseData(bufio.NewScanner(strings.NewReader(input)), true)
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

    got = TotalWinnings([]Bid{
        Bid { hand: []Value{c3, c2, cT, c3, cK}, wager: 765},
        Bid { hand: []Value{cT, c5, c5, joker, c5}, wager: 684},
        Bid { hand: []Value{cK, cK, c6, c7, c7}, wager: 28},
        Bid { hand: []Value{cK, cT, joker, joker, cT}, wager: 220},
        Bid { hand: []Value{cQ, cQ, cQ, joker, cA}, wager: 483},
    })
    expected = 5905

    if got != expected {
        t.Errorf("Expected %d, got %d.", expected, got)
    }
}
