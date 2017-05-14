class ScalaClassWithCheckedExceptions_1[E1 <: Exception] @throws[NullPointerException]("") () {
  @throws[E1]("") def bar() {}
  @throws[IllegalStateException]("") def baz(x: Int) {}
  // FIXME: scala/bug#7066
  // @throws[E2]("") def foo[E2 <: Exception] {}
}