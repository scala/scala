package lib;

public interface Interface<T> {

  T getField();

  default int fieldHash() {
    return getField().hashCode();
  }

  static final long CONST = 42;

  static <U> Interface<U> create(U field) {
    return new Interface<U>() {
      public U getField() { return field; }
    };
  }

}
