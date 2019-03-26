package nestpkg;

import java.io.PrintStream;

public class nest_1 {
  String name = "Outer name";

  protected class ProtInn {
    protected ProtInn() {
    }

    public void doSomething() {
      System.out.println("ProtInn " + nest_1.this.name);
    }
  }

  public class Inn {
    int x;

    public Inn(int n) {
      this.x = n;
    }

    public void doSomething() {
      System.out.println("Inn " + nest_1.this.name + " x: " + this.x);
    }
  }

  public static class best {

    public static class rest {
      public static rest test = new rest();
      public static int x = 10;

      public int inc(int n) {
        return n + 1;
      }
    }

  }

}