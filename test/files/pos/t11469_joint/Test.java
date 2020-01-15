import java.util.Optional;

public class Test{
  public abstract static class A<T> {
    public void m(Optional<? extends T> a) { }
  }
  public abstract static class B extends A<Object> {
    @Override
    public void m(Optional<?> a) { }
  }
}
