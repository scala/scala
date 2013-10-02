object Test {
  implicit def foo2bar(foo: Foo): Bar = foo.bar

  class Foo(val bar: Bar) {
    def testCoercion = {
      val a: this.type = this
      a.baz /* here, foo2bar is inferred  by the compiler, as expected */
    }
    // def testCoercionThis0 = baz
    // --> error: not found: value baz
    // PP: is that something we really want to work? Seems like sketchville.
    //
    // These work, so I moved this out of pending.
    def testCoercionThis1 = this.baz
    def testCoercionThis2 = (this: Foo).baz
  }

  class Bar { def baz = System.out.println("baz") }
}
