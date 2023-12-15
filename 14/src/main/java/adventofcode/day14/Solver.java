package adventofcode.day14;

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

  public enum Direction {
    NORTH, WEST, SOUTH, EAST
  }

  public static Map<Point, Point> generateShortcuts(List<String> input, Direction dir) {
    int length = input.size();
    Map<Point, Point> shortcuts = new HashMap<>();
    Point current;
    for (int row = 0; row < length; row++) {
      for (int col = 0; col < length; col++) {
        if (input.get(row).charAt(col) == '#') continue;
        current = new Point(row, col);
        int next;

        switch (dir) {
          case NORTH:
            for (next = row; next >= 0; next--) {
              if (input.get(next).charAt(col) == '#') break;
            }
            shortcuts.put(current, new Point(next + 1, col));
            break;
          case SOUTH:
            for (next = row; next < length; next++) {
              if (input.get(next).charAt(col) == '#') break;
            }
            shortcuts.put(current, new Point(next - 1, col));
            break;
          case WEST:
            for (next = col; next >= 0; next--) {
              if (input.get(row).charAt(next) == '#') break;
            }
            shortcuts.put(current, new Point(row, next + 1));
            break;
          case EAST:
            for (next = col; next < length; next++) {
              if (input.get(row).charAt(next) == '#') break;
            }
            shortcuts.put(current, new Point(row, next - 1));
            break;
        }
      }
    }
    return shortcuts;
  }

  private static void printShortcut(Map<Point, Point> shortcut) {
    System.out.println("[");
    shortcut.entrySet().stream()
      .sorted((r1, r2) -> { int res = r1.getKey().x - r2.getKey().x; return (res == 0) ? r1.getKey().y - r2.getKey().y : res; })
      .forEachOrdered(route -> {
        System.out.printf(
            "    (%d, %d) => (%d, %d)\n",
            route.getKey().x, route.getKey().y,
            route.getValue().x, route.getValue().y
        );
      });
    System.out.println("]");
  }

  private static void printRocks(List<Point> rocks) {
    System.out.print("[");
    rocks.stream()
      //.sorted((o1, o2) -> { int res = o1.x - o2.x; return (res == 0) ? o1.y - o2.y : res; })
      .forEachOrdered(rock -> System.out.printf("(%d, %d), ", rock.x, rock.y));
    System.out.println("]");
  }

  public static List<Point> parseData(List<String> input) {
    List<Point> rocks = new ArrayList<Point>();
    for (int row = 0; row < input.size(); row++) {
      for (int col = 0; col < input.size(); col++) {
        if (input.get(row).charAt(col) == 'O') {
          rocks.add(new Point(row, col));
        }
      }
    }
    return rocks;
  }

  public static void predict(List<Point> rocks, Map<Point, Point> shortcut) {
    //printShortcut(shortcut);
    for (final Point rock : rocks) {
      rock.setLocation(shortcut.get(rock));
    }
  }

  public static void resolve(List<Point> rocks, Direction dir) {
    int[] histogram = new int[10_000];
    for (final Point rock : rocks) {
      int hash = rock.x * 100 + rock.y;
      if (histogram[hash] > 0) {
        switch (dir) {
          case NORTH:
            rock.translate(histogram[hash], 0);
            break;
          case SOUTH:
            rock.translate(-histogram[hash], 0);
            break;
          case WEST:
            rock.translate(0, histogram[hash]);
            break;
          case EAST:
            rock.translate(0, -histogram[hash]);
            break;
        }
      }
      histogram[hash] += 1;
    }
  }

  public static String computeHash(List<Point> rocks) {
    StringBuilder sb = new StringBuilder();
    rocks.stream()
      .sorted((o1, o2) -> { int res = o1.x - o2.x; return (res == 0) ? o1.y - o2.y : res; })
      .forEachOrdered(rock -> {
        sb.append('(');
        sb.append(rock.x);
        sb.append(',');
        sb.append(rock.y);
        sb.append(')');
      });
    return sb.toString();
  }

  public static int spinTheBottle(List<String> input, int cycles) {
    int length = input.size();
    Map<Direction, Map<Point, Point>> shortcuts = new HashMap<>(4);
    for (Direction dir : Direction.values()) {
      shortcuts.put(dir, generateShortcuts(input, dir));
    }
    List<Point> rocks = parseData(input);

    Direction[] cycleOrder = new Direction[] {
      Direction.NORTH,
      Direction.WEST,
      Direction.SOUTH,
      Direction.EAST
    };
    Map<String, Integer> cache = new HashMap<>();
    for (int i = 0; i < cycles; i++) {
      for (int dirIndex = 0; dirIndex < 4; dirIndex++) {
        //predict(rocks, shortcuts.get(cycleOrder[dirIndex]));
        //resolve(rocks, cycleOrder[dirIndex]);
        int[] histogram = new int[10_000];
        for (final Point rock : rocks) {
          rock.setLocation(shortcuts.get(cycleOrder[dirIndex]).get(rock));
          int hash = rock.x * 100 + rock.y;
          if (histogram[hash] > 0) {
            switch (cycleOrder[dirIndex]) {
              case NORTH:
                rock.translate(histogram[hash], 0);
                break;
              case SOUTH:
                rock.translate(-histogram[hash], 0);
                break;
              case WEST:
                rock.translate(0, histogram[hash]);
                break;
              case EAST:
                rock.translate(0, -histogram[hash]);
                break;
            }
          }
          histogram[hash] += 1;
        }
      }
      String gridHash = computeHash(rocks);
      if (cache.containsKey(gridHash)) {
        int metaCycleLength = i - cache.get(gridHash);
        while (i < cycles - metaCycleLength) {
          i += metaCycleLength;
        }
      } else {
        cache.put(gridHash, i);
      }
    }

    int sum = 0;
    for (Point rock : rocks) {
      sum += length - rock.x;
    }
    return sum;
  }
}
