public class J {
  Object x;

  public J(Object x) {
    this.x = x;
  }

  public J(int x1, int x2, int x3, int x4, int x5, int x6) {
    this.x = null;
  }

  public String toString() {
    return "J:" + x.getClass();
  }
}