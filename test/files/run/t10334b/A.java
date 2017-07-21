public interface A<T> {
  public String apply(T s);
  public default String apply(String s) { return "hi"; }
}
