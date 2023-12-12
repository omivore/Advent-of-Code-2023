package spring

import (
    "fmt"
    "regexp"
    "strconv"
    "strings"
)

func ParseRow(line string) ([]string, []int) {
    parts := strings.Split(line, " ")
    var sets []int
    for _, set := range strings.Split(parts[1], ",") {
        count, _ := strconv.Atoi(set)
        sets = append(sets, count)
    }

    re := regexp.MustCompile("[?#]+")
    groups := re.FindAllString(parts[0], -1)

    return groups, sets
}

// Create a bunch of valid combos for the grouping and sets, and
// return the remaining portions.
func EnumerateRemainders(grouping string, sets []int) ([]string, [][]int) {
    fmt.Println("Remainders:", grouping, sets)

    mandatoryHashes := []int{}
    for i, char := range grouping {
        if char == '#' {
            mandatoryHashes = append(mandatoryHashes, i)
        }
    }

    remainders := []string{}
    remainingSets := [][]int{}
    var sb strings.Builder
    if len(sets) > 0 {
        EnumerateCombos:
        for i := 0; i <= len(grouping) - sets[0]; i++ {
            sb.Reset()
            for j := 0; j < i; j++ {
                sb.WriteString(".")
            }
            for j := 0; j < sets[0]; j++ {
                sb.WriteString("#")
            }
            if i + sets[0] < len(grouping) {
                sb.WriteString(".")
            }
            combo := sb.String()
            for _, mandatory := range mandatoryHashes {
                if mandatory < len(combo) && combo[mandatory] != '#' {
                    continue EnumerateCombos
                }
            }
            fmt.Println("Combo:", combo, "+", grouping[len(combo):])
            // Combo is valid! Hooray!
            remainders = append(remainders, grouping[len(combo):])
            remainingSets = append(remainingSets, sets[1:])
        }
    }
    // Don't forget the remainder where no combos are used! This
    // technically consumes all of them, but dosen't consume a set
    sb.Reset()
    for i := 0; i <= len(grouping); i++ {
        sb.WriteString(".")
    }
    combo := sb.String()
    canUseEmptyCombo := true
    for _, mandatory := range mandatoryHashes {
        if mandatory < len(combo) && combo[mandatory] != '#' {
            canUseEmptyCombo = false
        }
    }
    if canUseEmptyCombo {
        remainders = append(remainders, "")
        remainingSets = append(remainingSets, sets)
    }

    return remainders, remainingSets
}

func EnumerateValid(groups []string, sets []int) (count int) {
    fmt.Println("Valid:", groups, sets)
    if len(groups) == 0 && len(sets) == 0 {
        fmt.Println("+1")
        return 1
    } else if (len(groups) == 0 && len(sets) > 0) {
        fmt.Println("+0")
        return 0
    }

    remainders, remainingSets := EnumerateRemainders(groups[0], sets)
    fmt.Println(len(remainders), "Remainders:", remainders, remainingSets)
    for i := range remainders {
        var completeRemainder []string
        if remainders[i] == "" {
            completeRemainder = groups[1:]
        } else {
            completeRemainder = append([]string{remainders[i]} , groups[1:]...)
        }
        count += EnumerateValid(completeRemainder, remainingSets[i])
    }

    return
}
