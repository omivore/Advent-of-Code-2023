package adventofcode.day17;

import java.awt.Point;
import java.util.List;

public class MappedNode {
  public enum Direction {
    NORTH, WEST, SOUTH, EAST;

    private Direction opposite;

    static {
      NORTH.opposite = SOUTH;
      SOUTH.opposite = NORTH;
      EAST.opposite = WEST;
      WEST.opposite = EAST;
    }

    public Direction getOppositeDirection() {
      return opposite;
    }
  }

  List<String> input;
  int heatLoss;
  Point point;
  Direction to;
  int consecutiveDir;

  MappedNode parent;
  int totalLoss;

  public MappedNode(List<String> input, Point point, Direction to, int consecutiveDir) {
    this.input = input;
    this.point = point;
    this.heatLoss = Integer.parseInt(String.valueOf(input.get(point.x).charAt(point.y)));
    this.to = to;
    this.consecutiveDir = consecutiveDir;

    this.parent = null;
    this.totalLoss = Integer.MAX_VALUE;
  }

  public String toString() {
    return String.format("%d -> %d", heatLoss, totalLoss);
  }
}
