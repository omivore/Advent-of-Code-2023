package adventofcode.day13;

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
    List<String> lines = new ArrayList<String>();
    int total = 0;

    try (Stream<String> stream = Files.lines(inPath)) {
        Iterator<String> it = stream.iterator();
        String line;
        while (it.hasNext()) {
            line = it.next();
            if (line.equals("")) {
                total += Solver.summarizeFindings(lines);
                lines.clear();
            } else {
                lines.add(line);
            }
        }
        total += Solver.summarizeFindings(lines);
    }
    System.out.println(total);
  }
}
