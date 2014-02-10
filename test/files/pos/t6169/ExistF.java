public class ExistF<T extends ExistF<T>> {
  // java helpfully re-interprets ExistF<?> as ExistF<?0 extends ExistF<?0>>
  public ExistF<?> foo() { throw new RuntimeException(); }
}