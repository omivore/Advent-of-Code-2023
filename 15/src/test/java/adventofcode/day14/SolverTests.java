package adventofcode.day15;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.AbstractMap.SimpleEntry;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

class SolverTests {

  @ParameterizedTest
  @CsvSource({
    "HASH, 52",
    "rn=1, 30",
    "cm-, 253",
    "qp=3, 97",
    "cm=2, 47",
    "qp-, 14",
    "pc=4, 180",
    "ot=9, 9",
    "ab=5, 197",
    "pc-, 48",
    "pc=6, 214",
    "ot=7, 231",
    "rn, 0",
  })
  void convertToPriority(String in, int hash) {
    assertEquals(hash, Solver.hash(in));
  }

  @ParameterizedTest
  @CsvSource(delimiter = '|', value = {
    "rn=1 | 1",
    "rn=1,cm- | 1",
    "rn=1,cm-,qp=3 | 7",
    "rn=1,cm-,qp=3,cm=2 | 11",
    "rn=1,cm-,qp=3,cm=2,qp- | 5",
    "rn=1,cm-,qp=3,cm=2,qp-,pc=4 | 21",
    "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9 | 93",
    "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5 | 153",
    "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc- | 81",
    "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6 | 153",
    "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7 | 145",
  })
  void testInitializer(String in, int score) {
    assertEquals(score, Solver.initialize(in));
  }
}
