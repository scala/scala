class Outer {
  class Inner {
    def foo = {
      () => assert(!null.isInstanceOf[Outer.this.type])
    }
  }
}

object Test extends App {
  val outer = new Outer
  val inner = new outer.Inner
  inner.foo.apply()
}
