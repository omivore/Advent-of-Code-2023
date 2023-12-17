package adventofcode.day16;

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
    List<String> input = Arrays.asList(
      ".|...\\....",
      "|.-.\\.....",
      ".....|-...",
      "........|.",
      "..........",
      ".........\\",
      "..../.\\\\..",
      ".-.-/..|..",
      ".|....-|.\\",
      "..//.|...."
    );
    assertEquals(46, Solver.traverse(input, new Point(0, 0), Solver.Direction.EAST).size());
  }

  @Test
  void testMaximize() {
    List<String> input = Arrays.asList(
      ".|...\\....",
      "|.-.\\.....",
      ".....|-...",
      "........|.",
      "..........",
      ".........\\",
      "..../.\\\\..",
      ".-.-/..|..",
      ".|....-|.\\",
      "..//.|...."
    );
    assertEquals(51, Solver.maximize(input));
  }
}
