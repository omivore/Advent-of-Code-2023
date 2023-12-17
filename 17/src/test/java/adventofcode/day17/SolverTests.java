package adventofcode.day17;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.awt.Point;
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
  void testTraverse() {
    List<String> input;
    input = Arrays.asList(
      "2413432311323",
      "3215453535623",
      "3255245654254",
      "3446585845452",
      "4546657867536",
      "1438598798454",
      "4457876987766",
      "3637877979653",
      "4654967986887",
      "4564679986453",
      "1224686865563",
      "2546548887735",
      "4322674655533"
    );
    assertEquals(102, Solver.minimizeHeatLoss(input, false));
    assertEquals(94, Solver.minimizeHeatLoss(input, true));

    input = Arrays.asList(
      "111111111111",
      "999999999991",
      "999999999991",
      "999999999991",
      "999999999991"
    );
    assertEquals(71, Solver.minimizeHeatLoss(input, true));
  }
}
