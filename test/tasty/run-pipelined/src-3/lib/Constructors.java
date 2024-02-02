package lib;

public class Constructors {

  public static class CanonicalGeneric<T> {}
  public static class Canonical {}

  public static class SingleGeneric<T> {
    public SingleGeneric(T t) {}
  }

  public static class Single {
    public Single(int t) {}
  }

  public static class MultipleGeneric<T> {

    public MultipleGeneric(T t) {}

    public MultipleGeneric(T t, T u) {
      this(t);
    }

  }

  public static class Multiple {

    public Multiple(int t) {}

    public Multiple(int t, int u) {
      this(t);
    }

  }

}
