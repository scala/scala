class Outer<T> {
  class Inner {
  }
  static Outer<Integer>.Inner f() {
    return null;
  }
}
