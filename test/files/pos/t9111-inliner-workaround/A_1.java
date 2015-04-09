public class A_1 {
  public static class T { }

  public static class Inner {
    public static class T { }

    public void foo(T t) { }

    public T t = null;

    public class Deeper extends T { }
  }
}
