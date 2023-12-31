package rps

import (
    "bufio"
    "strings"
)

type Choice int
const (
    Rock Choice = iota
    Paper
    Scissors
)
type Outcome int
const (
    Win Outcome = iota
    Lose
    Tie
)

var referee = map[Choice]map[Choice]Outcome{
    Rock: map[Choice]Outcome{
        Rock: Tie,
        Paper: Lose,
        Scissors: Win,
    },
    Paper: map[Choice]Outcome{
        Rock: Win,
        Paper: Tie,
        Scissors: Lose,
    },
    Scissors: map[Choice]Outcome{
        Rock: Lose,
        Paper: Win,
        Scissors: Tie,
    },
}

type Round struct {
    you  Choice
    them Choice
}

func (r Round) Points() int {
    total := 0

    switch r.you {
    case Rock:
        total += 1
    case Paper:
        total += 2
    case Scissors:
        total += 3
    }

    switch referee[r.you][r.them] {
    case Win:
        total += 6
    case Lose:
        total += 0
    case Tie:
        total += 3
    }

    return total
}

func ParseData(scanner *bufio.Scanner) []Round {
    var rounds []Round
    for scanner.Scan() {
        moves := strings.Split(scanner.Text(), " ")
        if len(moves) != 2 {
            panic("Unexpected number of moves in input")
        }
        var you, them Choice
        switch moves[0] {
        case "A":
            them = Rock
        case "B":
            them = Paper
        case "C":
            them = Scissors
        }
        switch moves[1] {
        case "X":
            you = Rock
        case "Y":
            you = Paper
        case "Z":
            you = Scissors
        }

        rounds = append(rounds, Round{you: you, them: them})
    }
    return rounds
}
