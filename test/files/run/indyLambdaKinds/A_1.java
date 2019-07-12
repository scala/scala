import java.util.function.*;
import java.lang.annotation.Annotation;

public class A_1 {
  public final String m1(String x) { return "m1"; }
  public final static String m2(String x) { return "m2"; }
  public A_1(String x) { }

  public final BiFunction<A_1, String, String> a() { return A_1::m1; }
  public final Function<String, String> b() { return A_1::m2; }
  public final Function<String, A_1> c() { return A_1::new; }

  public final BiFunction<A_1, String, String> d(String x) { return (a, s) -> a.m1(s + x); }
  public final Function<String, String> e(String x) { return s -> A_1.m2(s + x); }
  public final Function<String, A_1> f(String x) { return s -> new A_1(s + x); }
}
