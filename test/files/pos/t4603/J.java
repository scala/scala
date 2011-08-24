// J.java
public class J<T> {
  public static void f(java.lang.Class<? extends J> cls) { }
  // correctly it should be like this, and then it would work.
  // unfortunately that doesn't mean we don't have to deal with it.
  // public static void f(java.lang.Class<? extends J<?>> cls) { }
}
