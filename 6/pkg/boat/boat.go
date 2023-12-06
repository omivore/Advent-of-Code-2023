package boat

import (
    "bufio"
    "math"
    "strconv"
    "strings"
)

type Race struct {
    time float64
    distance float64
}

func (r Race) HoldToBeat() (valid []int) {
    // duration = 7, length = 9
    // length < (duration - held) * held
    // length < -held^2 + held*duration
    // held^2 - duration*held + length < 0
    // top = ((duration + sqrt(duration^2 - 4*length)) / 2)
    // bottom = ((duration - sqrt(duration^2 - 4*length)) / 2)
    // bottom < options < top

    top := (r.time + math.Sqrt(math.Pow(r.time, 2) - 4 * r.distance)) / 2
    bot := (r.time - math.Sqrt(math.Pow(r.time, 2) - 4 * r.distance)) / 2
    whole_top := int(math.Ceil(top - 1))
    whole_bot := int(math.Floor(bot + 1))

    for i := whole_bot; i <= whole_top; i++ {
        valid = append(valid, i)
    }

    return
}

func ParseData(scanner *bufio.Scanner, kerning bool) (races []Race) {
    if !scanner.Scan() {
        panic("Failed to read first line")
    }
    times := strings.Fields(scanner.Text())[1:]
    if !scanner.Scan() {
        panic("Failed to read first line")
    }
    distances := strings.Fields(scanner.Text())[1:]

    if len(times) != len(distances) {
        panic("Got uneven number of times and distances")
    }

    var time, distance float64
    var err error
    if kerning {
        time, err = strconv.ParseFloat(strings.Join(times, ""), 64)
        if err != nil { panic("Could not parse times.") }
        distance, err = strconv.ParseFloat(strings.Join(distances, ""), 64)
        if err != nil { panic("Could not parse distances.") }
        races = append(races, Race { time: time, distance: distance })
    } else {
        for i := 0; i < len(times); i++ {
            time, err = strconv.ParseFloat(times[i], 64)
            if err != nil { panic("Could not parse times.") }
            distance, err = strconv.ParseFloat(distances[i], 64)
            if err != nil { panic("Could not parse distances.") }
            races = append(races, Race{ time: time, distance: distance })
        }
    }
    return
}
