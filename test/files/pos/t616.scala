object testImplicit {
  implicit def foo2bar(foo: Foo): Bar = foo.bar
  class Foo(val bar: Bar) {
    def testCoercion = {val a = this; a.baz} // here, foo2bar is inferred by the compiler, as expected
    //def testCoercionThisImplicit = baz  // --> error: not found: value baz
    def testCoercionThisExplicit: Any = this.baz  // --> error: value baz is not a  member of Foo
  }
  trait Bar { def baz: Unit }
}
// mentioned before: http://thread.gmane.org/gmane.comp.lang.scala/2038,
// but couldn't find a bug report
