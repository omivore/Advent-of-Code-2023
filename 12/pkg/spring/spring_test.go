package spring

import (
    "fmt"
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

func CheckHashmaps(t *testing.T, expected map[int]bool, got map[int]bool) {
    for k, v := range expected {
        if got[k] != v {
            t.Errorf("Expected %t for %d", v, k)
        }
    }
    for k, v := range got {
        if expected[k] != v {
            t.Errorf("Expected %t for %d", v, k)
        }
    }
}

func TestParseRow(t *testing.T) {
    var expectedGroups, gotGroups []string
    var expectedSets, gotSets []int

    expectedGroups = []string{"???", "###"}
    expectedSets = []int{1, 1, 3}
    gotGroups, gotSets = ParseRow("???.### 1,1,3")
    CheckArray(t, expectedGroups, gotGroups)
    CheckArray(t, expectedSets, gotSets)

    expectedGroups = []string{"??", "??", "?##"}
    expectedSets = []int{1, 1, 3}
    gotGroups, gotSets = ParseRow(".??..??...?##. 1,1,3")
    CheckArray(t, expectedGroups, gotGroups)
    CheckArray(t, expectedSets, gotSets)

    expectedGroups = []string{"?#?#?#?#?#?#?#?"}
    expectedSets = []int{1, 3, 1, 6}
    gotGroups, gotSets = ParseRow("?#?#?#?#?#?#?#? 1,3,1,6")
    CheckArray(t, expectedGroups, gotGroups)
    CheckArray(t, expectedSets, gotSets)

    expectedGroups = []string{"????", "#", "#"}
    expectedSets = []int{4, 1, 1}
    gotGroups, gotSets = ParseRow("????.#...#... 4,1,1")
    CheckArray(t, expectedGroups, gotGroups)
    CheckArray(t, expectedSets, gotSets)

    expectedGroups = []string{"????", "######", "#####"}
    expectedSets = []int{1, 6, 5}
    gotGroups, gotSets = ParseRow("????.######..#####. 1,6,5")
    CheckArray(t, expectedGroups, gotGroups)
    CheckArray(t, expectedSets, gotSets)

    expectedGroups = []string{"?###????????"}
    expectedSets = []int{3, 2, 1}
    gotGroups, gotSets = ParseRow("?###???????? 3,2,1")
    CheckArray(t, expectedGroups, gotGroups)
    CheckArray(t, expectedSets, gotSets)
}

func TestUnfold(t *testing.T) {
    var expectedGroups, gotGroups []string
    var expectedSets, gotSets []int

    expectedGroups = []string{"???", "###????", "###????", "###????", "###????", "###"}
    expectedSets = []int{1, 1, 3, 1, 1, 3, 1, 1, 3, 1, 1, 3, 1, 1, 3}
    gotGroups, gotSets = UnfoldRow("???.### 1,1,3")
    CheckArray(t, expectedGroups, gotGroups)
    CheckArray(t, expectedSets, gotSets)
}

func TestEnumerateValid(t *testing.T) {
    var inputGroup []string
    var inputSet []int
    var expected, got int

    inputGroup = []string{"???", "###"}
    inputSet = []int{1, 1, 3}
    fmt.Println("Test 1")
    got = EnumerateValid(inputGroup, inputSet)
    expected = 1
    if expected != got {
        t.Errorf("Expected %d, got %d", expected, got)
    }

    inputGroup = []string{"??", "??", "?##"}
    inputSet = []int{1, 1, 3}
    expected = 4
    fmt.Println("Test 2")
    got = EnumerateValid(inputGroup, inputSet)
    if expected != got {
        t.Errorf("Expected %d, got %d", expected, got)
    }

    inputGroup = []string{"?#?#?#?#?#?#?#?"}
    inputSet = []int{1, 3, 1, 6}
    fmt.Println("Test 3")
    got = EnumerateValid(inputGroup, inputSet)
    expected = 1
    if expected != got {
        t.Errorf("Expected %d, got %d", expected, got)
    }

    inputGroup = []string{"????", "#", "#"}
    inputSet = []int{4, 1, 1}
    fmt.Println("Test 4")
    got = EnumerateValid(inputGroup, inputSet)
    expected = 1
    if expected != got {
        t.Errorf("Expected %d, got %d", expected, got)
    }

    inputGroup = []string{"????", "######", "#####"}
    inputSet = []int{1, 6, 5}
    fmt.Println("Test 5")
    got = EnumerateValid(inputGroup, inputSet)
    expected = 4
    if expected != got {
        t.Errorf("Expected %d, got %d", expected, got)
    }

    inputGroup = []string{"?###????????"}
    inputSet = []int{3, 2, 1}
    fmt.Println("Test 6")
    got = EnumerateValid(inputGroup, inputSet)
    expected = 10
    if expected != got {
        t.Errorf("Expected %d, got %d", expected, got)
    }





    inputGroup = []string{"???", "###????", "###????", "###????", "###????", "###"}
    inputSet = []int{1, 1, 3, 1, 1, 3, 1, 1, 3, 1, 1, 3, 1, 1, 3}
    fmt.Println("Test 2.1")
    got = EnumerateValid(inputGroup, inputSet)
    expected = 1
    if expected != got {
        t.Errorf("Expected %d, got %d", expected, got)
    }

    inputGroup = []string{"??", "??", "?##", "?", "??", "??", "?##", "?", "??", "??", "?##", "?", "??", "??", "?##", "?", "??", "??", "?##"}
    inputSet = []int{1, 1, 3, 1, 1, 3, 1, 1, 3, 1, 1, 3, 1, 1, 3}
    expected = 16384
    fmt.Println("Test 2.2")
    got = EnumerateValid(inputGroup, inputSet)
    if expected != got {
        t.Errorf("Expected %d, got %d", expected, got)
    }

    inputGroup = []string{"?#?#?#?#?#?#?#???#?#?#?#?#?#?#???#?#?#?#?#?#?#???#?#?#?#?#?#?#???#?#?#?#?#?#?#?"}
    inputSet = []int{1, 3, 1, 6, 1, 3, 1, 6, 1, 3, 1, 6, 1, 3, 1, 6, 1, 3, 1, 6}
    fmt.Println("Test 2.3")
    got = EnumerateValid(inputGroup, inputSet)
    expected = 1
    if expected != got {
        t.Errorf("Expected %d, got %d", expected, got)
    }

    inputGroup = []string{"????", "#", "#", "?????", "#", "#", "?????", "#", "#", "?????", "#", "#", "?????", "#", "#"}
    inputSet = []int{4, 1, 1, 4, 1, 1, 4, 1, 1, 4, 1, 1, 4, 1, 1}
    fmt.Println("Test 2.4")
    got = EnumerateValid(inputGroup, inputSet)
    expected = 16
    if expected != got {
        t.Errorf("Expected %d, got %d", expected, got)
    }

    inputGroup = []string{"????", "######", "#####", "?", "????", "######", "#####", "?", "????", "######", "#####", "?", "????", "######", "#####", "?", "????", "######", "#####"}
    inputSet = []int{1, 6, 5, 1, 6, 5, 1, 6, 5, 1, 6, 5, 1, 6, 5}
    fmt.Println("Test 2.5")
    got = EnumerateValid(inputGroup, inputSet)
    expected = 2500
    if expected != got {
        t.Errorf("Expected %d, got %d", expected, got)
    }

    inputGroup = []string{"?###??????????###??????????###??????????###??????????###????????"}
    inputSet = []int{3, 2, 1, 3, 2, 1, 3, 2, 1, 3, 2, 1, 3, 2, 1}
    fmt.Println("Test 2.6")
    got = EnumerateValid(inputGroup, inputSet)
    expected = 506250
    if expected != got {
        t.Errorf("Expected %d, got %d", expected, got)
    }
}
