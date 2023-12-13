package adventofcode.day13;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

class SolverTests {

  @Test
  void parsesData() {
    List<String> input, expectedRows, expectedCols, gotRows, gotCols;

    input = Arrays.asList(
      "#.##..##.",
      "..#.##.#.",
      "##......#",
      "##......#",
      "..#.##.#.",
      "..##..##.",
      "#.#.##.#."
    );
    expectedRows = Arrays.asList(
      "#.##..##.",
      "..#.##.#.",
      "##......#",
      "##......#",
      "..#.##.#.",
      "..##..##.",
      "#.#.##.#."
    );
    expectedCols = Arrays.asList(
      "#.##..#",
      "..##...",
      "##..###",
      "#....#.",
      ".#..#.#",
      ".#..#.#",
      "#....#.",
      "##..###",
      "..##..."
    );
    gotRows = Solver.parseRows(input);
    gotCols = Solver.parseColumns(input);

    assertEquals(expectedRows.size(), gotRows.size());
    assertEquals(expectedCols.size(), gotCols.size());
    assertArrayEquals(expectedRows.toArray(), gotRows.toArray());
    assertArrayEquals(expectedCols.toArray(), gotCols.toArray());

    input = Arrays.asList(
      "#...##..#",
      "#....#..#",
      "..##..###",
      "#####.##.",
      "#####.##.",
      "..##..###",
      "#....#..#"
    );
    expectedRows = Arrays.asList(
      "#...##..#",
      "#....#..#",
      "..##..###",
      "#####.##.",
      "#####.##.",
      "..##..###",
      "#....#..#"
    );
    expectedCols = Arrays.asList(
      "##.##.#",
      "...##..",
      "..####.",
      "..####.",
      "#..##..",
      "##....#",
      "..####.",
      "..####.",
      "###..##"
    );
    gotRows = Solver.parseRows(input);
    gotCols = Solver.parseColumns(input);

    assertEquals(expectedRows.size(), gotRows.size());
    assertEquals(expectedCols.size(), gotCols.size());
    assertArrayEquals(expectedRows.toArray(), gotRows.toArray());
    assertArrayEquals(expectedCols.toArray(), gotCols.toArray());
  }

  @Test
  void summarizesFindings() {
    List<String> input;
    int expected, got;

    input = Arrays.asList(
      "#.##..##.",
      "..#.##.#.",
      "##......#",
      "##......#",
      "..#.##.#.",
      "..##..##.",
      "#.#.##.#."
    );
    assertEquals(5, Solver.summarizeFindings(input));

    input = Arrays.asList(
      "#...##..#",
      "#....#..#",
      "..##..###",
      "#####.##.",
      "#####.##.",
      "..##..###",
      "#....#..#"
    );
    assertEquals(400, Solver.summarizeFindings(input));
  }

  @Test
  void summarizesSmudges() {
    List<String> input;
    int expected, got;

    input = Arrays.asList(
      "#.##..##.",
      "..#.##.#.",
      "##......#",
      "##......#",
      "..#.##.#.",
      "..##..##.",
      "#.#.##.#."
    );
    assertEquals(300, Solver.summarizeSmudges(input));

    input = Arrays.asList(
      "#...##..#",
      "#....#..#",
      "..##..###",
      "#####.##.",
      "#####.##.",
      "..##..###",
      "#....#..#"
    );
    assertEquals(100, Solver.summarizeSmudges(input));
  }
}
