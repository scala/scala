import test.generic.*;

public class J_2 {
  public static <A> void foo(T1<A> x) {
    // x.m1();
  }

  public static void main(String[] args) {
    Bug4891.main(null);
    T1<Object> x = new C2<Object>();
    foo(x);
  }
}
