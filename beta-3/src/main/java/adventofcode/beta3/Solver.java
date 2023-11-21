package adventofcode.beta3;

import java.lang.IllegalArgumentException;
import java.util.Arrays;
import java.util.HashSet;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

/** Solves the Advent of Code problem.
 */
public class Solver {

  public static List<String[]> parseData(List<String> input) {
    List<String[]> results = new ArrayList<String[]>();

    for (final String string : input) {
      int size = string.length() / 2;
      String first = string.substring(0, size);
      String second = string.substring(size, string.length());
      results.add(new String[]{first, second});
    }

    return results;
  }

  public static List<String[]> parseData2(List<String> input) {
    List<String[]> results = new ArrayList<String[]>();

    for (final String string : input) {
      int size = string.length() / 2;
      String first = string.substring(0, size);
      String second = string.substring(size, string.length());
      results.add(new String[]{first, second});
    }

    return results;
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
