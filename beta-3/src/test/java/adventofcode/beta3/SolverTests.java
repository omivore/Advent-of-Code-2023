package adventofcode.beta3;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

class SolverTests {

  @ParameterizedTest
  @CsvSource({
    "vJrwpWtwJgWrhcsFMMfFFhFp, vJrwpWtwJgWr, hcsFMMfFFhFp",
    "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL, jqHRNqRjqzjGDLGL, rsFMfFZSrLrFZsSL",
    "PmmdzqPrVvPwwTWBwg, PmmdzqPrV, vPwwTWBwg",
    "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn, wMqvLMZHhHMvwLH, jbvcjnnSBnvTQFn",
    "ttgJtRGJQctTZtZT, ttgJtRGJ, QctTZtZT",
    "CrZsJsPPZsGzwwsLwLmpwMDw, CrZsJsPPZsGz, wwsLwLmpwMDw",
  })
  void parsesData(String input, String rucksack1, String rucksack2) {
    String[] expected = new String[]{rucksack1, rucksack2};
    assertArrayEquals(expected, Solver.parseData(input));
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
