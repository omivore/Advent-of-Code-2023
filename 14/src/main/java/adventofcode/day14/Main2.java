package adventofcode.day14;

import java.io.IOException;
import java.lang.IllegalArgumentException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.stream.*;

/**
 * Main CLI application for the second half of the problem.
 */
public class Main2 {

  /**
   * Main CLI entrypoint.
   */
  public static void main(String[] args) throws IOException {
    if (args[0] == null || args[0].trim().isEmpty()) {
      throw new IllegalArgumentException("No input file provided.");
    }

    Path inPath = Paths.get(args[0]);
    List<String> lines = Files.readAllLines(inPath, StandardCharsets.UTF_8);
    System.out.println(Solver.spinTheBottle(lines, 1_000_000_000));
  }
}
