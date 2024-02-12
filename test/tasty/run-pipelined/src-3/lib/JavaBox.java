package lib;

public class JavaBox<T> {
  private final T value;

  public JavaBox(T value) {
    this.value = value;
  }

  public T value() {
    return value;
  }
}
