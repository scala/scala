public class ExistIndir<T extends String, U extends T> {
  // java helpfully re-interprets ExistIndir<?> as ExistIndir<? extends String>
  public ExistIndir<?, ?> foo() { throw new RuntimeException(); }
}
