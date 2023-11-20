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

var fixer = map[Choice]map[Outcome]Choice{
    Rock: map[Outcome]Choice{
        Win: Paper,
        Lose: Scissors,
        Tie: Rock,
    },
    Paper: map[Outcome]Choice{
        Win: Scissors,
        Lose: Rock,
        Tie: Paper,
    },
    Scissors: map[Outcome]Choice{
        Win: Rock,
        Lose: Paper,
        Tie: Scissors,
    },
}

type Strategy struct {
    them   Choice
    result Outcome
}

func (s Strategy) Implement() Round {
    return Round{you: fixer[s.them][s.result], them: s.them}
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

func ParseData(scanner *bufio.Scanner) []Strategy {
    var strats []Strategy
    for scanner.Scan() {
        moves := strings.Split(scanner.Text(), " ")
        if len(moves) != 2 {
            panic("Unexpected number of moves in input")
        }
        var result Outcome
        var them Choice
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
            result = Lose
        case "Y":
            result = Tie
        case "Z":
            result = Win
        }

        strats = append(strats, Strategy{result: result, them: them})
    }
    return strats
}
