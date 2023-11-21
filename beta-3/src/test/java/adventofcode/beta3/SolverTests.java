package adventofcode.beta3;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;

import java.util.Arrays;
import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

class SolverTests {

  @Test
  void parsesData() {
    List<String> input = Arrays.asList(
      "vJrwpWtwJgWrhcsFMMfFFhFp",
      "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
      "PmmdzqPrVvPwwTWBwg",
      "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
      "ttgJtRGJQctTZtZT",
      "CrZsJsPPZsGzwwsLwLmpwMDw"
    );

    List<String[]> expected = Arrays.asList(
      new String[]{"vJrwpWtwJgWr", "hcsFMMfFFhFp"},
      new String[]{"jqHRNqRjqzjGDLGL", "rsFMfFZSrLrFZsSL"},
      new String[]{"PmmdzqPrV", "vPwwTWBwg"},
      new String[]{"wMqvLMZHhHMvwLH", "jbvcjnnSBnvTQFn"},
      new String[]{"ttgJtRGJ", "QctTZtZT"},
      new String[]{"CrZsJsPPZsGz", "wwsLwLmpwMDw"}
    );
    List<String[]> got = Solver.parseData(input);

    assertEquals(expected.size(), got.size());

    for (int i = 0; i < expected.size(); i++) {
      assertArrayEquals(expected.get(i), got.get(i));
    }
  }

  @ParameterizedTest
  @CsvSource({
    "vJrwpWtwJgWr, hcsFMMfFFhFp, p",
    "jqHRNqRjqzjGDLGL, rsFMfFZSrLrFZsSL, L",
    "PmmdzqPrV, vPwwTWBwg, P",
    "wMqvLMZHhHMvwLH, jbvcjnnSBnvTQFn, v",
    "ttgJtRGJ, QctTZtZT, t",
    "CrZsJsPPZsGz, wwsLwLmpwMDw, s",
  })
  void findsOddOne(String in1, String in2, String odd) {
    assertEquals(odd, Solver.findOdd(in1, in2));
  }

  @ParameterizedTest
  @CsvSource({
    "p, 16",
    "L, 38",
    "P, 42",
    "v, 22",
    "t, 20",
    "s, 19",
  })
  void convertToPriority(char value, int priority) {
    assertEquals(Solver.calculatePriority(value), priority);
  }
}
