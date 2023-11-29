package adventofcode.beta3;

import java.lang.IllegalArgumentException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/** Solves the Advent of Code problem.
 */
public class Solver {

  /** Take in a list of strings to create a list of pairs of rucksacks.
   */
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

  /** Take in a list of string to create a list of groups of rucksacks.
   */
  public static List<String[]> parseData2(List<String> input) {
    List<String[]> results = new ArrayList<String[]>();

    int i = 0;
    String[] group = new String[3];
    for (final String string : input) {
      group[i] = string;
      i += 1;
      if (i == 3) {
        results.add(group);
        group = new String[3];
        i = 0;
      }
    }

    return results;
  }

  /** Given a number of String sacks, find the shared attribute amongst them all.
   */
  public static String findOdd(String... sacks) {
    if (sacks.length < 1) {
      return "";
    }

    Set<String> shared = new HashSet<String>(Arrays.asList(sacks[0].split("")));
    for (int i = 1; i < sacks.length; i++) {
      Set<String> sack = new HashSet<String>(Arrays.asList(sacks[i].split("")));
      shared.retainAll(sack);
    }

    return shared.toArray(new String[0])[0];
  }

  /** Converts a letter to its priority value.
   */
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
