package lib;

public class Bounds {

  public static class C {}

  public static interface I {}

  public static class A extends C implements I {}

  public static <T extends C & I> void m(T t) {}
  public static <T extends C & I> void m_ARRAY(T[] t) {}

}
