class A {
  new Test.B() { }

  new Test.B() {
    override def m(a: java.util.Optional[_ <: Object]): Unit = { }
  }

  new Test.B() {
    override def m(a: java.util.Optional[_]): Unit = { }
  }
}
