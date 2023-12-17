package adventofcode.day16;

import java.awt.Point;
import java.io.IOException;
import java.lang.IllegalArgumentException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.stream.*;

/**
 * Main CLI application for the first half of the problem.
 */
public class Main1 {

  /**
   * Main CLI entrypoint.
   */
  public static void main(String[] args) throws IOException {
    if (args[0] == null || args[0].trim().isEmpty()) {
      throw new IllegalArgumentException("No input file provided.");
    }

    Path inPath = Paths.get(args[0]);
    List<String> lines = Files.readAllLines(inPath, StandardCharsets.UTF_8);
    int result;
    result = Solver.traverse(lines, new Point(0, 0), Solver.Direction.EAST).size();
    System.out.println(result);
  }
}
