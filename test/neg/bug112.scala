object T {
  class A {
    class B;
  }
  type C = A#B;
  class D extends C;
}
