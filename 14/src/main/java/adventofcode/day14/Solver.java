package adventofcode.day14;

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
  public static int countLoad(List<String> input) {
    int total = 0;
    for (int col = 0; col < input.get(0).length(); col++) {
      int canScore = input.size();
      for (int row = 0; row < input.size(); row++) {
        if (input.get(row).charAt(col) == 'O') {
          total += canScore;
          canScore -= 1;
        } else if (input.get(row).charAt(col) == '#') {
          canScore = input.size() - row - 1;
        }
      }
    }
    return total;
  }

  /** Take in a list of strings to create a list of rows.
   */
  public static SimpleEntry<List<Set<Integer>>, List<SimpleEntry<Integer, Integer>>> tiltAndConvert(List<String> input) {
    List<Set<Integer>> map;
    Set<Integer> column;
    List<SimpleEntry<Integer, Integer>> stops = new ArrayList<SimpleEntry<Integer, Integer>>();
    map = new ArrayList<Set<Integer>>();
    int endRow;
    for (int col = 0; col < input.get(0).length(); col++) {
      column = new HashSet<Integer>();
      endRow = input.size();
      for (int row = 0; row < input.size(); row++) {
        if (input.get(row).charAt(col) == 'O') {
          column.add(endRow - 1);
          endRow -= 1;
        } else if (input.get(row).charAt(col) == '#') {
          stops.add(new SimpleEntry<Integer, Integer>(row, col));
          endRow = input.size() - row - 1;
        }
      }
      map.add(column);
    }
    return new SimpleEntry<List<Set<Integer>>, List<SimpleEntry<Integer, Integer>>>(map, stops);
  }

  /** Turns the positions of the stops clockwise.
   */
  public static List<SimpleEntry<Integer, Integer>> rotateStops(List<SimpleEntry<Integer, Integer>> stops, int totalRows) {
    return stops.stream()
      .map((stop) -> new SimpleEntry<Integer, Integer>(stop.getValue(), totalRows - 1 - stop.getKey()))
      .sorted((o1, o2) -> { int res = o1.getValue().compareTo(o2.getValue()); return (res == 0) ? o2.getKey().compareTo(o1.getKey()) : res; })
      .collect(Collectors.toList());
  }

  public static List<Set<Integer>> clockwiseSpin(List<Set<Integer>> maps, List<SimpleEntry<Integer, Integer>> stops, int newRowWidth) {
    List<Set<Integer>> newMaps = new ArrayList<Set<Integer>>();
    for (int i = 0; i < maps.size(); i++) {
      newMaps.add(new HashSet<Integer>());
    }
    int end, endRow;
    Map<Integer, Integer> lowestStop = new HashMap<Integer, Integer>();
    for (int i = 0; i < stops.size(); i++) {
      SimpleEntry<Integer, Integer> stop = stops.get(i);
      if (stop.getKey() > lowestStop.getOrDefault(stop.getValue(), -1)) {
        lowestStop.put(stop.getValue(), stop.getKey());
      }
      if (i + 1 < stops.size() && stops.get(i + 1).getValue() == stop.getValue()) {
        end = stops.get(i + 1).getKey();
      } else {
        end = -1;
      }
      endRow = (maps.size() - 1) - (end + 1);
      for (int j = stop.getKey() - 1; j > end; j--) {
        if (maps.get(j).contains(stop.getValue())) {
          newMaps.get(stop.getValue()).add(endRow);
          endRow -= 1;
        }
      }
    }
    for (int col = 0; col < newRowWidth; col++) {
      end = lowestStop.getOrDefault(col, -1);
      endRow = (maps.size() - 1) - (end + 1);
      for (int row = maps.size() - 1; row > end; row--) {
        if (maps.get(row).contains(col)) {
          newMaps.get(col).add(endRow);
          endRow -= 1;
        }
      }
    }
    return newMaps;
  }

  public static int spinTheBottle(List<String> input, int cycles) {
    int length = input.size();
    SimpleEntry<List<Set<Integer>>, List<SimpleEntry<Integer, Integer>>> start = tiltAndConvert(input);
    List<Set<Integer>> maps = start.getKey();

    List<List<SimpleEntry<Integer, Integer>>> all_stops = new ArrayList<List<SimpleEntry<Integer, Integer>>>(4);
    all_stops.add(start.getValue());
    all_stops.add(rotateStops(all_stops.get(0), length));
    all_stops.add(rotateStops(all_stops.get(1), length));
    all_stops.add(rotateStops(all_stops.get(2), length));
    all_stops.set(0, rotateStops(all_stops.get(3), length));


    for (int i = 1; i < 4; i++) {
      maps = clockwiseSpin(maps, all_stops.get(i), length);
    }
    System.out.println("Doing " + cycles + " cycles");
    long last = System.nanoTime();
    for (int i = 1; i < cycles; i++) {
      for (int j = 0; j < 4; j++) {
        maps = clockwiseSpin(maps, all_stops.get(j), length);
      }
      if (i % 1_000_000 == 0) {
        System.out.println(i);
        System.out.println((System.nanoTime() - last) / 1_000_000);
        last = System.nanoTime();
      }
    }

    int sum = 0;
    int worth;
    for (int i = 0; i < maps.size(); i++) {
      worth = maps.size() - i;
      sum += worth * maps.get(i).size();
    }

    return sum;
  }
}
