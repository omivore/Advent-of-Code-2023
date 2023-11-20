package adventofcode.beta3;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class SolverTests {

  @Test
  @DisplayName("1 + 1 = 2")
  void addsTwoNumbers() {
    Solver solver = new Solver();
    assertEquals(2, solver.add(1, 1), "1 + 1 should equal 2");
  }
}
