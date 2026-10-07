package p;

class C1<X> {
  class I extends C1<X> { }
}

class C2<X> {
  class I {
    C2<X> foo() { return null; }
  }
}
