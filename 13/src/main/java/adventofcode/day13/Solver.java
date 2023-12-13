package adventofcode.day13;

import java.lang.IllegalArgumentException;
import java.lang.StringBuilder;
import java.util.AbstractMap.SimpleEntry;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

/** Solves the Advent of Code problem.
 */
public class Solver {

  /** Take in a list of strings to create a list of rows.
   */
  public static List<String> parseRows(List<String> input) {
    return input;
  }

  /** Take in a list of string to create a list of columns.
   */
  public static List<String> parseColumns(List<String> input) {
    List<String> results = new ArrayList<String>();

    StringBuilder sb = new StringBuilder();
    for (int i = 0; i < input.get(0).length(); i++) {
      sb.delete(0, sb.length());
      for (final String string : input) {
        sb.append(string.charAt(i));
      }
      results.add(sb.toString());
    }

    return results;
  }

  /** Find the point of reflection in a list of strings
   */
  public static int findReflection(List<String> ashes) {
    if (ashes.size() < 2) {
      throw new IllegalArgumentException("ashes must be at least two elements long");
    }

    Map<Integer, ArrayDeque<String>> hypotheses = new HashMap<Integer, ArrayDeque<String>>(ashes.size());
    ArrayDeque<String> history = new ArrayDeque<String>();
    history.push(ashes.get(0));
    hypotheses.put(1, history.clone());

    for (int i = 1; i < ashes.size(); i++) {
      hypotheses.put(i, history.clone());
      int foundReflection = updateHypotheses(hypotheses, ashes.get(i));
      if (foundReflection > 0) {
        return foundReflection;
      }
      history.push(ashes.get(i));
    }

    if (!hypotheses.isEmpty()) {
      if (hypotheses.size() == 1) {
        return hypotheses.keySet().toArray(new Integer[0])[0];
      } else {
        throw new IllegalArgumentException("ashes has multiple points of reflection");
      }
    } else {
      return -1;
    }
  }

  /** Remove disproved hypotheses, advance still-valid ones, and return any proven ones
   */
  public static int updateHypotheses(Map<Integer, ArrayDeque<String>> hypotheses, String evidence) {
    String record;
    Entry<Integer, ArrayDeque<String>> hypothesis;
    Iterator<Entry<Integer, ArrayDeque<String>>> it = hypotheses.entrySet().iterator();
    while (it.hasNext()) {
      hypothesis = it.next();
      record = hypothesis.getValue().pop();
      if (!record.equals(evidence)) {
        it.remove();
        continue;
      }

      if (hypothesis.getValue().isEmpty()) {
        return hypothesis.getKey();
      }
    }

    return -1;
  }

  /** Scan for reflection and summarize the findings appropriately.
   */
  public static int summarizeFindings(List<String> input) {
    int answer = -1;
    List<String> rows = parseRows(input);
    answer = findReflection(rows);
    if (answer > 0) {
      return 100 * answer;
    }
    List<String> cols = parseColumns(input);
    answer = findReflection(cols);
    if (answer > 0) {
      return answer;
    }
    throw new IllegalArgumentException("couldn't determine answer");
  }

  /** Find the point of reflection in a list of strings knowing there's a smudge
   */
  public static int findSmudge(List<String> ashes) {
    if (ashes.size() < 2) {
      throw new IllegalArgumentException("ashes must be at least two elements long");
    }

    Map<Integer, SimpleEntry<ArrayDeque<String>, Integer>> hypotheses = new HashMap<Integer, SimpleEntry<ArrayDeque<String>, Integer>>(ashes.size());
    ArrayDeque<String> history = new ArrayDeque<String>();
    history.push(ashes.get(0));
    hypotheses.put(1, new SimpleEntry<ArrayDeque<String>, Integer>(history.clone(), 0));

    for (int i = 1; i < ashes.size(); i++) {
      hypotheses.put(i, new SimpleEntry<ArrayDeque<String>, Integer>(history.clone(), 0));
      int foundReflection = updateHypothesesWithError(hypotheses, ashes.get(i));
      if (foundReflection > 0) {
        return foundReflection;
      }
      history.push(ashes.get(i));
    }

    if (!hypotheses.isEmpty()) {
      for (Entry<Integer, SimpleEntry<ArrayDeque<String>, Integer>> hypothesis : hypotheses.entrySet()) {
        if (hypothesis.getValue().getValue() == 1) {
          return hypothesis.getKey();
        }
      }
    }
    return -1;
  }

  /** Remove disproved hypotheses, advance still-valid ones, and return any proven ones
   */
  public static int updateHypothesesWithError(
    Map<Integer, SimpleEntry<ArrayDeque<String>, Integer>> hypotheses,
    String evidence
  ) {
    String record;
    int diff;
    Entry<Integer, SimpleEntry<ArrayDeque<String>, Integer>> hypothesis;
    Iterator<Entry<Integer, SimpleEntry<ArrayDeque<String>, Integer>>> it = hypotheses.entrySet().iterator();
    while (it.hasNext()) {
      hypothesis = it.next();
      record = hypothesis.getValue().getKey().pop();

      diff = distance(record, evidence);
      hypothesis.getValue().setValue(hypothesis.getValue().getValue() + diff);
      if (hypothesis.getValue().getValue() > 1) {
        it.remove();
        continue;
      }

      if (hypothesis.getValue().getKey().isEmpty()) {
        if (hypothesis.getValue().getValue() == 1) {
          return hypothesis.getKey();
        } else if (hypothesis.getValue().getValue() == 0) {
          // I don't care if this is the og perfect reflection
          it.remove();
          continue;
        }
      }
    }

    return -1;
  }

  /** Calculates the number of differences between two strings.
   */
  public static int distance(String a, String b) {
    if (a.length() != b.length()) {
        throw new IllegalArgumentException("strings are different lengths");
    }
    int total = 0;
    for (int i = 0; i < a.length(); i++) {
      if (a.charAt(i) == b.charAt(i)) {
        continue;
      } else {
        total += 1;
      }
    }
    return total;
  }

  /** Scan for smudges and summarize the findings appropriately.
   */
  public static int summarizeSmudges(List<String> input) {
    int answer = -1;
    List<String> rows = parseRows(input);
    answer = findSmudge(rows);
    if (answer > 0) {
      return 100 * answer;
    }
    List<String> cols = parseColumns(input);
    answer = findSmudge(cols);
    if (answer > 0) {
      return answer;
    }
    throw new IllegalArgumentException("couldn't determine answer");
  }
}
