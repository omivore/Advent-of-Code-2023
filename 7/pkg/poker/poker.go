package poker

import (
    "bufio"
    "slices"
    "strconv"
    "strings"
)

type Value int

const (
    joker Value = iota
    c2
    c3
    c4
    c5
    c6
    c7
    c8
    c9
    cT
    cJ
    cQ
    cK
    cA
    hHi
    h1p
    h2p
    h3k
    hFh
    h4k
    h5k
)

type Bid struct {
    hand []Value
    wager int
}

func CalculateType(hand []Value) Value {
    cardFreq := make(map[Value]int)
    jokers := 0
    for _, card := range hand {
        if card != joker {
            cardFreq[card] += 1
        } else {
            jokers += 1
        }
    }

    freqFreq := make(map[int]int)
    for _, freq := range cardFreq {
        freqFreq[freq] += 1
    }

    if freqFreq[5] == 1 {
        return h5k
    } else if freqFreq[4] == 1 {
        if jokers == 1 {
            return h5k
        } else {
            return h4k
        }
    } else if freqFreq[3] == 1 {
        switch jokers {
        case 1:
            return h4k
        case 2:
            return h5k
        default:
            if freqFreq[2] == 1 {
                return hFh
            } else {
                return h3k
            }
        }
    } else if freqFreq[2] == 2 {
        if jokers == 1 {
            return hFh
        } else {
            return h2p
        }
    } else if freqFreq[2] == 1 {
        switch jokers {
        case 1:
            return h3k
        case 2:
            return h4k
        case 3:
            return h5k
        default:
            return h1p
        }
    } else {
        switch jokers {
        case 1:
            return h1p
        case 2:
            return h3k
        case 3:
            return h4k
        case 4, 5:
            return h5k
        default:
            return hHi
        }
    }
}

func CalculateValue(hand []Value) (values []Value) {
    values = append([]Value{CalculateType(hand)}, hand...)
    return
}

func TotalWinnings(bids []Bid) (total int) {
    slices.SortFunc(
        bids,
        func(a, b Bid) int {
            return slices.Compare(
                CalculateValue(a.hand),
                CalculateValue(b.hand),
            )
        },
    )

    for i, bid := range bids {
        total += (i + 1) * bid.wager
    }

    return
}

func ParseData(scanner *bufio.Scanner, useJoker bool) (bids []Bid) {
    for scanner.Scan() {
        bid_data := strings.Split(scanner.Text(), " ")
        if len(bid_data) != 2 {
            panic("Expected two inputs.")
        }

        hand := ParseHand(strings.Split(bid_data[0], ""), useJoker)
        wager, err := strconv.Atoi(bid_data[1])
        if err != nil {
            panic("Could not parse wager value")
        }

        bids = append(
            bids,
            Bid {
                hand: hand,
                wager: wager,
            },
        )
    }

    return
}

func ParseHand(hand_data []string, useJoker bool) (hand []Value) {
    for _, card_data := range hand_data {
        hand = append(hand, MapStringToValue(card_data, useJoker))
    }

    return
}

func MapStringToValue(in string, useJoker bool) Value {
    switch in {
    case "2":
        return c2
    case "3":
        return c3
    case "4":
        return c4
    case "5":
        return c5
    case "6":
        return c6
    case "7":
        return c7
    case "8":
        return c8
    case "9":
        return c9
    case "T":
        return cT
    case "J":
        if useJoker { return joker } else { return cJ }
    case "Q":
        return cQ
    case "K":
        return cK
    case "A":
        return cA
    default:
        panic("Unrecognized string")
    }
}
