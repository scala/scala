package lib;

public class Bounds {

  public static class C {}

  public static interface I {}

  public static class A extends C implements I {}

  public static <T extends C & I> void m(T t) {}
  public static <T extends C & I> void m_ARRAY(T[] t) {}
  public static void m_WILDCARD_SUB(Class<? extends C> c) {}
  public static void m_WILDCARD_SUP(Class<? super A> c) {}


  public static class F<T extends F<T>> {}

  public static interface FExtra<T extends F<T>> {}

  public static class G extends F<G> implements FExtra<G> {}

  public static <T extends F<T> & FExtra<T>> void f(T t) {}
  public static <T extends F<T> & FExtra<T>> void f_ARRAY(T[] t) {}
  public static void f_WILDCARD_SUB(Class<? extends F<?>> c) {}
  public static void f_WILDCARD_SUP(Class<? super G> c) {}

}
