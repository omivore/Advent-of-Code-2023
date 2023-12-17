package adventofcode.day17;

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

  public static String hash(MappedNode.Direction to, int consecutiveDir, Point point) {
    return String.format("(%d, %d):%d%s", point.x, point.y, consecutiveDir, to.toString());
  }

  public static int minimizeHeatLoss(List<String> input, boolean useUltraCrucible) {
    Map<String, MappedNode> nodes = new HashMap<>();

    Set<String> travelled = new HashSet<>();
    List<String> toTravel = new ArrayList<>();

    String initialHash1 = hash(MappedNode.Direction.EAST, 0, new Point(0, 0));
    MappedNode initialNode1 = new MappedNode(input, new Point(0, 0), MappedNode.Direction.EAST, 0);
    initialNode1.totalLoss = 0;
    nodes.put(initialHash1, initialNode1);
    toTravel.add(initialHash1);

    String initialHash2 = hash(MappedNode.Direction.SOUTH, 0, new Point(0, 0));
    MappedNode initialNode2 = new MappedNode(input, new Point(0, 0), MappedNode.Direction.SOUTH, 0);
    initialNode2.totalLoss = 0;
    nodes.put(initialHash2, initialNode2);
    toTravel.add(initialHash2);

    while (!toTravel.isEmpty()) {
      String currentHash = toTravel.get(0);
      MappedNode current = nodes.get(currentHash);
      for (String nodeHash : toTravel) {
        if (nodes.get(nodeHash).totalLoss < current.totalLoss) {
          currentHash = nodeHash;
          current = nodes.get(currentHash);
        }
      }
      for (String pointHash : getNeighbors(nodes, input, current, useUltraCrucible)) {
        MappedNode neighbor = nodes.get(pointHash);
        neighbor.totalLoss = Math.min(
          neighbor.totalLoss,
          current.totalLoss + neighbor.heatLoss
        );
        if (!travelled.contains(pointHash) && !toTravel.contains(pointHash)) {
          toTravel.add(pointHash);
        }
      }
      travelled.add(currentHash);
      toTravel.remove(currentHash);
    }

    int min = Integer.MAX_VALUE;
    for (MappedNode node : nodes.values()) {
      if (node.point.equals(new Point(input.size() - 1, input.get(0).length() - 1))) {
        if (useUltraCrucible) {
          if (node.consecutiveDir >= 4) {
            min = Math.min(min, node.totalLoss);
          }
        } else {
          min = Math.min(min, node.totalLoss);
        }
      }
    }

    return min;
  }

  public static List<String> getNeighbors(
      Map<String, MappedNode> ref,
      List<String> map,
      MappedNode current,
      boolean useUltraCrucible
  ) {
    List<String> neighbors = new ArrayList<>();

    for (MappedNode.Direction dir : MappedNode.Direction.values()) {
      if (useUltraCrucible) {
        if (current.to == dir.getOppositeDirection()) {
          continue;
        }
        if (current.consecutiveDir < 4 && current.to != dir) {
          continue;
        }
        if (current.to == dir && current.consecutiveDir >= 10) {
          continue;
        }
      } else {
        if (current.to == dir.getOppositeDirection()) {
          continue;
        }
        if (current.to == dir && current.consecutiveDir >= 3) {
          continue;
        }
      }



      Point newPoint = new Point(current.point);
      switch (dir) {
      case NORTH:
        newPoint.translate(-1, 0);
        break;
      case SOUTH:
        newPoint.translate(1, 0);
        break;
      case WEST:
        newPoint.translate(0, -1);
        break;
      case EAST:
        newPoint.translate(0, 1);
        break;
      }
      if (
          newPoint.x < map.size()
          && newPoint.x >= 0
          && newPoint.y < map.get(0).length()
          && newPoint.y >= 0
      ) {
        String pointHash = hash(
          dir,
          (current.to == dir) ? current.consecutiveDir + 1 : 1,
          newPoint
        );
        if (!ref.containsKey(pointHash)) {
          MappedNode newlyDiscovered = new MappedNode(
            map,
            newPoint,
            dir,
            (current.to == dir) ? current.consecutiveDir + 1 : 1
          );
          ref.put(pointHash, newlyDiscovered);
        }
        neighbors.add(pointHash);
      }
    }

    return neighbors;
  }
}
