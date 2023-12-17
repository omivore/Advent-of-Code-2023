package adventofcode.day16;

import java.awt.Point;
import java.lang.IllegalArgumentException;
import java.lang.StringBuilder;
import java.util.AbstractMap.SimpleEntry;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.*;

/** Solves the Advent of Code problem.
 */
public class Solver {

  public enum Direction {
    NORTH, WEST, SOUTH, EAST
  }

  public static List<SimpleEntry<Point, Direction>> stepPath(List<String> input, Point current, Direction dir) {
    List<SimpleEntry<Point, Direction>> steps = new ArrayList<>();
    SimpleEntry<Point, Direction> next = new SimpleEntry<Point, Direction>(new Point(current), dir);
    switch (dir) {
      case NORTH:
        next.getKey().translate(-1, 0);
        break;
      case SOUTH:
        next.getKey().translate(1, 0);
        break;
      case WEST:
        next.getKey().translate(0, -1);
        break;
      case EAST:
        next.getKey().translate(0, 1);
        break;
    }
    if (
        next.getKey().x < input.size()
        && next.getKey().x >= 0
        && next.getKey().y < input.get(0).length()
        && next.getKey().y >= 0
    ) {
      steps.add(next);
    }
    return steps;
  }

  public static List<SimpleEntry<Point, Direction>> next(List<String> input, Point current, Direction dir) {
    char tileChar = input.get(current.x).charAt(current.y);
    switch (tileChar) {
    case '.':
      switch (dir) {
      case NORTH:
        return stepPath(input, current, Direction.NORTH);
      case EAST:
        return stepPath(input, current, Direction.EAST);
      case SOUTH:
        return stepPath(input, current, Direction.SOUTH);
      case WEST:
        return stepPath(input, current, Direction.WEST);
      }
    case '/':
      switch (dir) {
      case NORTH:
        return stepPath(input, current, Direction.EAST);
      case EAST:
        return stepPath(input, current, Direction.NORTH);
      case SOUTH:
        return stepPath(input, current, Direction.WEST);
      case WEST:
        return stepPath(input, current, Direction.SOUTH);
      }
    case '\\':
      switch (dir) {
      case NORTH:
        return stepPath(input, current, Direction.WEST);
      case EAST:
        return stepPath(input, current, Direction.SOUTH);
      case SOUTH:
        return stepPath(input, current, Direction.EAST);
      case WEST:
        return stepPath(input, current, Direction.NORTH);
      }
    case '-':
      switch (dir) {
      case NORTH:
      case SOUTH:
        return Stream.of(
            stepPath(input, current, Direction.WEST),
            stepPath(input, current, Direction.EAST)
          )
          .flatMap(Collection::stream)
          .toList();
      case EAST:
        return stepPath(input, current, Direction.EAST);
      case WEST:
        return stepPath(input, current, Direction.WEST);
      }
    case '|':
      switch (dir) {
      case NORTH:
        return stepPath(input, current, Direction.NORTH);
      case SOUTH:
        return stepPath(input, current, Direction.SOUTH);
      case EAST:
      case WEST:
        return Stream.of(
            stepPath(input, current, Direction.NORTH),
            stepPath(input, current, Direction.SOUTH)
          )
          .flatMap(Collection::stream)
          .toList();
      }
    }

    throw new IllegalArgumentException("Could not derive correct direction to move in.");
  }

  public static Set<Point> traverse(List<String> input, Point start, Direction dir) {
    List<SimpleEntry<Point, Direction>> travelled = new ArrayList<>();
    ArrayDeque<SimpleEntry<Point, Direction>> toTravel = new ArrayDeque<>();
    toTravel.offer(new SimpleEntry(start, dir));

    while (!toTravel.isEmpty()) {
      SimpleEntry<Point, Direction> current = toTravel.pop();
      travelled.add(current);
      List<SimpleEntry<Point, Direction>> nexts = next(input, current.getKey(), current.getValue());
      for (SimpleEntry<Point, Direction> step : nexts) {
        if (!travelled.contains(step)) {
          toTravel.offer(step);
        }
      }
    }

    return travelled.stream()
      .map(entry -> entry.getKey())
      .collect(Collectors.toSet());
  }

  public static int maximize(List<String> input) {
    int max = 0;
    int result;
    int rows = input.size();
    int cols = input.get(0).length();
    for (int i = 0; i < cols; i++) {
      result = traverse(input, new Point(0, i), Direction.SOUTH).size();
      max = (result > max) ? result : max;

      result = traverse(input, new Point(rows - 1, i), Direction.NORTH).size();
      max = (result > max) ? result : max;
    }
    for (int i = 0; i < rows; i++) {
      result = traverse(input, new Point(i, 0), Direction.EAST).size();
      max = (result > max) ? result : max;

      result = traverse(input, new Point(i, cols - 1), Direction.WEST).size();
      max = (result > max) ? result : max;
    }

    return max;
  }
}
