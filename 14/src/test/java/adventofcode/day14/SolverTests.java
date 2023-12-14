package adventofcode.day14;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.AbstractMap.SimpleEntry;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

class SolverTests {

  @Test
  void testCountLoad() {
    List<String> input;
    int expected, got;

    input = Arrays.asList(
      "O....#....",
      "O.OO#....#",
      ".....##...",
      "OO.#O....O",
      ".O.....O#.",
      "O.#..O.#.#",
      "..O..#O..O",
      ".......O..",
      "#....###..",
      "#OO..#...."
    );
    got = Solver.countLoad(input);
    expected = 136;

    assertEquals(expected, got);
  }

  @Test
  void testTiltAndConvert() {
    List<String> input;
    SimpleEntry<List<Set<Integer>>, List<SimpleEntry<Integer, Integer>>> got;
    List<Set<Integer>> gotMaps;
    List<SimpleEntry<Integer, Integer>> gotStops;

    input = Arrays.asList(
      "O....#....",
      "O.OO#....#",
      ".....##...",
      "OO.#O....O",
      ".O.....O#.",
      "O.#..O.#.#",
      "..O..#O..O",
      ".......O..",
      "#....###..",
      "#OO..#...."
    );
    List<Set<Integer>> expectedMaps = Arrays.asList(
        new HashSet<Integer>(Arrays.asList(9, 8, 7, 6)),
        new HashSet<Integer>(Arrays.asList(9, 8, 7)),
        new HashSet<Integer>(Arrays.asList(9, 3, 2)),
        new HashSet<Integer>(Arrays.asList(9)),
        new HashSet<Integer>(Arrays.asList(7)),
        new HashSet<Integer>(Arrays.asList(6)),
        new HashSet<Integer>(Arrays.asList(6)),
        new HashSet<Integer>(Arrays.asList(9, 3)),
        new HashSet<Integer>(Arrays.asList()),
        new HashSet<Integer>(Arrays.asList(7, 3))
    );
    List<SimpleEntry<Integer, Integer>> expectedStops = Arrays.asList(
        new SimpleEntry(8, 0),
        new SimpleEntry(9, 0),
        new SimpleEntry(5, 2),
        new SimpleEntry(3, 3),
        new SimpleEntry(1, 4),
        new SimpleEntry(0, 5),
        new SimpleEntry(2, 5),
        new SimpleEntry(6, 5),
        new SimpleEntry(8, 5),
        new SimpleEntry(9, 5),
        new SimpleEntry(2, 6),
        new SimpleEntry(8, 6),
        new SimpleEntry(5, 7),
        new SimpleEntry(8, 7),
        new SimpleEntry(4, 8),
        new SimpleEntry(1, 9),
        new SimpleEntry(5, 9)
    );

    got = Solver.tiltAndConvert(input);

    gotMaps = got.getKey();
    for (int i = 0; i < expectedMaps.size(); i++) {
      assertEquals(expectedMaps.get(i), gotMaps.get(i));
    }
    gotStops = got.getValue();
    for (int i = 0; i < expectedStops.size(); i++) {
      assertEquals(expectedStops.get(i), gotStops.get(i));
    }
  }

  @Test
  void testRotateStops() {
    List<SimpleEntry<Integer, Integer>> input, expected, got;

    input = Arrays.asList(
        new SimpleEntry(8, 0),
        new SimpleEntry(9, 0),
        new SimpleEntry(5, 2),
        new SimpleEntry(3, 3),
        new SimpleEntry(1, 4),
        new SimpleEntry(0, 5),
        new SimpleEntry(2, 5),
        new SimpleEntry(6, 5),
        new SimpleEntry(8, 5),
        new SimpleEntry(9, 5),
        new SimpleEntry(2, 6),
        new SimpleEntry(8, 6),
        new SimpleEntry(5, 7),
        new SimpleEntry(8, 7),
        new SimpleEntry(4, 8),
        new SimpleEntry(1, 9),
        new SimpleEntry(5, 9)
    );
    expected = Arrays.asList(
        new SimpleEntry(5, 0),
        new SimpleEntry(0, 0),
        new SimpleEntry(7, 1),
        new SimpleEntry(6, 1),
        new SimpleEntry(5, 1),
        new SimpleEntry(0, 1),
        new SimpleEntry(5, 3),
        new SimpleEntry(9, 4),
        new SimpleEntry(7, 4),
        new SimpleEntry(2, 4),
        new SimpleEntry(8, 5),
        new SimpleEntry(3, 6),
        new SimpleEntry(6, 7),
        new SimpleEntry(5, 7),
        new SimpleEntry(9, 8),
        new SimpleEntry(4, 8),
        new SimpleEntry(5, 9)
    );
    got = Solver.rotateStops(input, 10);
    for (int i = 0; i < expected.size(); i++) {
      assertEquals(expected.get(i), got.get(i));
    }

    input = Arrays.asList(
        new SimpleEntry(5, 0),
        new SimpleEntry(0, 0),
        new SimpleEntry(7, 1),
        new SimpleEntry(6, 1),
        new SimpleEntry(5, 1),
        new SimpleEntry(0, 1),
        new SimpleEntry(5, 3),
        new SimpleEntry(9, 4),
        new SimpleEntry(7, 4),
        new SimpleEntry(2, 4),
        new SimpleEntry(8, 5),
        new SimpleEntry(3, 6),
        new SimpleEntry(6, 7),
        new SimpleEntry(5, 7),
        new SimpleEntry(9, 8),
        new SimpleEntry(4, 8),
        new SimpleEntry(5, 9)
    );
    expected = Arrays.asList(
        new SimpleEntry(8, 0),
        new SimpleEntry(4, 0),
        new SimpleEntry(5, 1),
        new SimpleEntry(4, 2),
        new SimpleEntry(1, 2),
        new SimpleEntry(7, 3),
        new SimpleEntry(1, 3),
        new SimpleEntry(9, 4),
        new SimpleEntry(7, 4),
        new SimpleEntry(3, 4),
        new SimpleEntry(1, 4),
        new SimpleEntry(0, 4),
        new SimpleEntry(8, 5),
        new SimpleEntry(6, 6),
        new SimpleEntry(4, 7),
        new SimpleEntry(1, 9),
        new SimpleEntry(0, 9)
    );
    got = Solver.rotateStops(input, 10);
    for (int i = 0; i < expected.size(); i++) {
      assertEquals(expected.get(i), got.get(i));
    }
  }

  @Test
  void testWashingMachine() {
    List<Set<Integer>> inMap, expected, got;
    List<SimpleEntry<Integer, Integer>> inStops;

    inStops = Arrays.asList(
        new SimpleEntry(5, 0),
        new SimpleEntry(0, 0),
        new SimpleEntry(7, 1),
        new SimpleEntry(6, 1),
        new SimpleEntry(5, 1),
        new SimpleEntry(0, 1),
        new SimpleEntry(5, 3),
        new SimpleEntry(9, 4),
        new SimpleEntry(7, 4),
        new SimpleEntry(2, 4),
        new SimpleEntry(8, 5),
        new SimpleEntry(3, 6),
        new SimpleEntry(6, 7),
        new SimpleEntry(5, 7),
        new SimpleEntry(9, 8),
        new SimpleEntry(4, 8),
        new SimpleEntry(5, 9)
    );
    inMap = Arrays.asList(
        new HashSet<Integer>(Arrays.asList(9, 8, 7, 6)),
        new HashSet<Integer>(Arrays.asList(9, 8, 7)),
        new HashSet<Integer>(Arrays.asList(9, 3, 2)),
        new HashSet<Integer>(Arrays.asList(9)),
        new HashSet<Integer>(Arrays.asList(7)),
        new HashSet<Integer>(Arrays.asList(6)),
        new HashSet<Integer>(Arrays.asList(6)),
        new HashSet<Integer>(Arrays.asList(9, 3)),
        new HashSet<Integer>(Arrays.asList()),
        new HashSet<Integer>(Arrays.asList(7, 3))
    );
    expected = Arrays.asList(
        new HashSet<Integer>(Arrays.asList()),
        new HashSet<Integer>(Arrays.asList()),
        new HashSet<Integer>(Arrays.asList(9)),
        new HashSet<Integer>(Arrays.asList(9, 3, 2)),
        new HashSet<Integer>(Arrays.asList()),
        new HashSet<Integer>(Arrays.asList()),
        new HashSet<Integer>(Arrays.asList(9, 5, 4)),
        new HashSet<Integer>(Arrays.asList(9, 8, 7, 2)),
        new HashSet<Integer>(Arrays.asList(9, 8)),
        new HashSet<Integer>(Arrays.asList(9, 8, 7, 6, 3))
    );
    got = Solver.clockwiseSpin(inMap, inStops, 10);

    for (int i = 0; i < expected.size(); i++) {
      assertEquals(new HashSet<>(expected.get(i)), new HashSet<>(got.get(i)));
    }

    inStops = Arrays.asList(
        new SimpleEntry(8, 0),
        new SimpleEntry(4, 0),
        new SimpleEntry(5, 1),
        new SimpleEntry(4, 2),
        new SimpleEntry(1, 2),
        new SimpleEntry(7, 3),
        new SimpleEntry(1, 3),
        new SimpleEntry(9, 4),
        new SimpleEntry(7, 4),
        new SimpleEntry(3, 4),
        new SimpleEntry(1, 4),
        new SimpleEntry(0, 4),
        new SimpleEntry(8, 5),
        new SimpleEntry(6, 6),
        new SimpleEntry(4, 7),
        new SimpleEntry(1, 9),
        new SimpleEntry(0, 9)
    );
    inMap = Arrays.asList(
        new HashSet<Integer>(Arrays.asList()),
        new HashSet<Integer>(Arrays.asList()),
        new HashSet<Integer>(Arrays.asList(9)),
        new HashSet<Integer>(Arrays.asList(9, 3, 2)),
        new HashSet<Integer>(Arrays.asList()),
        new HashSet<Integer>(Arrays.asList()),
        new HashSet<Integer>(Arrays.asList(9, 5, 4)),
        new HashSet<Integer>(Arrays.asList(9, 8, 7, 2)),
        new HashSet<Integer>(Arrays.asList(9, 8)),
        new HashSet<Integer>(Arrays.asList(9, 8, 7, 6, 3))
    );
    expected = Arrays.asList(
        new HashSet<Integer>(Arrays.asList()),
        new HashSet<Integer>(Arrays.asList()),
        new HashSet<Integer>(Arrays.asList(7, 4)),
        new HashSet<Integer>(Arrays.asList(7, 1)),
        new HashSet<Integer>(Arrays.asList(5)),
        new HashSet<Integer>(Arrays.asList(9)),
        new HashSet<Integer>(Arrays.asList(2)),
        new HashSet<Integer>(Arrays.asList(4, 3)),
        new HashSet<Integer>(Arrays.asList(9, 8, 7)),
        new HashSet<Integer>(Arrays.asList(7, 6, 5, 4, 3, 2))
    );
    got = Solver.clockwiseSpin(inMap, inStops, inMap.size());

    for (int i = 0; i < expected.size(); i++) {
      assertEquals(new HashSet<>(expected.get(i)), new HashSet<>(got.get(i)));
    }
  }

  @Test
  void testSpinTheBottle() {
    int got;
    List<String> input = Arrays.asList(
      "O....#....",
      "O.OO#....#",
      ".....##...",
      "OO.#O....O",
      ".O.....O#.",
      "O.#..O.#.#",
      "..O..#O..O",
      ".......O..",
      "#....###..",
      "#OO..#...."
    );
    got = Solver.spinTheBottle(input, 1);
    assertEquals(87, got);

    got = Solver.spinTheBottle(input, 2);
    assertEquals(69, got);

    got = Solver.spinTheBottle(input, 3);
    assertEquals(69, got);

    got = Solver.spinTheBottle(input, 1_000_000_000);
    assertEquals(64, got);
  }
}
