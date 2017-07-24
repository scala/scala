class Outer {
  class Inner {
    def foo: Unit = assert(Outer.this ne null)
  }
}

