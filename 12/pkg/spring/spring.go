package spring

import (
    "regexp"
    "strconv"
    "strings"
)

var re = regexp.MustCompile("[?#]+")

func ParseRow(line string) ([]string, []int) {
    parts := strings.Split(line, " ")
    var sets []int
    for _, set := range strings.Split(parts[1], ",") {
        count, _ := strconv.Atoi(set)
        sets = append(sets, count)
    }

    groups := re.FindAllString(parts[0], -1)

    return groups, sets
}

func UnfoldRow(line string) (newGroups []string, newSets []int) {
    parts := strings.Split(line, " ")

    var sets, unfoldedSets []int
    for _, set := range strings.Split(parts[1], ",") {
        count, _ := strconv.Atoi(set)
        sets = append(sets, count)
    }
    for i := 0; i < 5; i++ {
        unfoldedSets = append(unfoldedSets, sets...)
    }

    var sb strings.Builder
    for i := 0; i < 5; i++ {
        sb.WriteString(parts[0])
        sb.WriteString("?")
    }
    unfoldedGroupsString := sb.String()
    unfoldedGroupsString = unfoldedGroupsString[:len(unfoldedGroupsString) - 1]
    groups := re.FindAllString(unfoldedGroupsString, -1)

    return groups, unfoldedSets
}

// Create a bunch of valid combos for the grouping and sets, and
// return the remaining portions.
func EnumerateRemainders(grouping string, sets []int) ([]string, [][]int) {
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

func SerializeSet(groups []string, sets []int) string {
    var sb strings.Builder
    for _, group := range groups {
        sb.WriteString(group)
        sb.WriteString(".")
    }
    sb.WriteString(" ")
    for _, set := range sets {
        sb.WriteString(strconv.Itoa(set))
        sb.WriteString(",")
    }

    return sb.String()
}

func EnumerateValid(groups []string, sets []int, cache map[string]int) (count int) {
    serializedId := SerializeSet(groups, sets)
    if len(groups) == 0 && len(sets) == 0 {
        return 1
    } else if (len(groups) == 0 && len(sets) > 0) {
        return 0
    }

    if v, ok := cache[serializedId]; ok {
        return v
    }

    remainders, remainingSets := EnumerateRemainders(groups[0], sets)
    for i := range remainders {
        var completeRemainder []string
        if remainders[i] == "" {
            completeRemainder = groups[1:]
        } else {
            completeRemainder = append([]string{remainders[i]} , groups[1:]...)
        }
        count += EnumerateValid(completeRemainder, remainingSets[i], cache)
    }

    cache[serializedId] = count
    return
}
