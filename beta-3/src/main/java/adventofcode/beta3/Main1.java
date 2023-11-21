package adventofcode.beta3;

import java.io.IOException;
import java.lang.IllegalArgumentException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.stream.Collectors;

public class Main1 {

  public static void main(String[] args) throws IOException {
    if (args[0] == null || args[0].trim().isEmpty()) {
        throw new IllegalArgumentException("No input file provided.");
    }

    Path inPath = Paths.get(args[0]);
    List<String> lines = Files.readAllLines(inPath, StandardCharsets.UTF_8);
    List<Integer> priorities = Solver.parseData(lines)
      .stream()
      .map(sacks -> Solver.findOdd(sacks[0], sacks[1]))
      .map(string -> string.charAt(0))
      .map(Solver::calculatePriority)
      .collect(Collectors.toList());

    int total = priorities.stream().mapToInt(Integer::intValue).sum();
    System.out.println(total);
  }
}
