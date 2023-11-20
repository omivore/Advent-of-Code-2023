package adventofcode.beta3;

public class Main {

  public static void main(String[] args) {
    Solver solver = new Solver();
    System.out.format("Hello %d world!\n", solver.add(1, Integer.parseInt(args[0])));
  }
}
