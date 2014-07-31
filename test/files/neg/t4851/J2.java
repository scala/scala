public class J2<T> {
  T x;

  public J2(T x) {
    this.x = x;
  }

  public String toString() {
    return "J2:" + x.getClass();
  }
}
