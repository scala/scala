public class Tuppel_1<T> {
  private Tuppel_1(){}

  public static <A> Tuppel_1<A> get() {
    return new Tuppel_1<A>() {};
  }
}