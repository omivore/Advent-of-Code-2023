package adventofcode.day15;

import java.awt.Point;
import java.lang.IllegalArgumentException;
import java.lang.StringBuilder;
import java.util.AbstractMap.SimpleEntry;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.stream.Collectors;

/** Solves the Advent of Code problem.
 */
public class Solver {

  /** Take in a list of strings to create a list of rows.
   */
  public static int hash(String in) {
      int total = 0;
      for (int i = 0; i < in.length(); i++) {
          total += (int) in.charAt(i);
          total *= 17;
          total %= 256;
      }

      return total;
  }

  public static void execute(Map<Integer, List<SimpleEntry<String, Integer>>> boxes, String step) {
      String label;
      int box, focus;
      if (step.contains("=")) {
          String[] parts = step.split("=");
          label = parts[0];
          focus = Integer.parseInt(parts[1]);
          box = hash(label);

          alreadyExists: {
            for (final SimpleEntry<String, Integer> lens : boxes.get(box)) {
                if (lens.getKey().equals(label)) {
                    lens.setValue(focus);
                    break alreadyExists;
                }
            }
            boxes.get(box).add(new SimpleEntry(label, focus));
          }
      } else {
        String[] parts = step.split("-");
        label = parts[0];
        box = hash(label);
        boxes.get(box).removeIf(lens -> lens.getKey().equals(label));
      }
  }

  public static int score(Map<Integer, List<SimpleEntry<String, Integer>>> boxes) {
    int total = 0;
    for (Entry<Integer, List<SimpleEntry<String, Integer>>> box : boxes.entrySet()) {
      for (int slot = 0; slot < box.getValue().size(); slot++) {
        total += (box.getKey() + 1) * (slot + 1) * box.getValue().get(slot).getValue();
      }
    }
    return total;
  }

  public static int initialize(String input) {
    Map<Integer, List<SimpleEntry<String, Integer>>> boxes = new HashMap<>(256);
    for (int i = 0; i < 256; i++) {
      boxes.put(i, new ArrayList<SimpleEntry<String, Integer>>());
    }

    for (String step : input.split(",")) {
      execute(boxes, step);
    }
    return score(boxes);
  }
}
