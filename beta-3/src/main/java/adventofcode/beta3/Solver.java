package adventofcode.beta3;

import java.lang.IllegalArgumentException;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

/** Solves the Advent of Code problem.
 */
public class Solver {

  public static String[] parseData(String input) {
    int size = input.length() / 2;
    String first = input.substring(0, size);
    String second = input.substring(size, input.length());

    return new String[]{first, second};
  }

  public static String findOdd(String first, String second) {
    Set<String> firstRucksack = new HashSet<String>(Arrays.asList(first.split("")));
    Set<String> secondRucksack = new HashSet<String>(Arrays.asList(second.split("")));

    firstRucksack.retainAll(secondRucksack);
    return firstRucksack.toArray(new String[0])[0];
  }

  public static int calculatePriority(char value) {
    String map = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
    int priority = map.indexOf(value);
    if (priority == -1) {
      throw new IllegalArgumentException("value is not in alphabet");
    } else {
      return priority + 1;
    }
  }
}
