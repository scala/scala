package tastytest

object TestInner extends Suite("TestInner") {

  test(assert(Inner.Foo.Bar != null))
  test(assert(new Inner.Foo(){}.isInstanceOf[Inner.Foo]))

}
