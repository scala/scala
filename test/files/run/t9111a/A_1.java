public final class A_1 {
  public static final class T { public int x = 1; }
  public static final class Inner {
    public static final class T { public int x = 2; }
    public T newT() { return new T(); }
  }
}
