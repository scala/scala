// javaVersion: 21+

public class A_1 {
  public int f(Object s) {
    switch(s) {
      case Res.R -> {
        return 1;
      }
      default -> {
        return 3;
      }
    }
  }
  static enum Res {
    R
  }
}
