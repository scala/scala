public class Exist<T extends String> {
  // java helpfully re-interprets Exist<?> as Exist<? extends String>
  public Exist<?> foo() { throw new RuntimeException(); }
}