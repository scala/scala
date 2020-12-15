object Test extends App {

  // expr.isInstanceOf[Singleton] is true iff the expression has a singleton type
  // However, expr.asInstanceOf[Singleton] is erased to expr.asInstanceOf[Any] so it never throws
  // as discussed in https://docs.scala-lang.org/sips/minutes/2017-12-06-sip-minutes.html

  val foo: String = "foo"
  assert(foo.isInstanceOf[Singleton])
  assert((foo: String).isInstanceOf[Singleton])
  foo.asInstanceOf[Singleton]
  (foo: String).asInstanceOf[Singleton]

  val bar: "foo" = "foo"
  assert(bar.isInstanceOf[Singleton])
  assert((bar: String).isInstanceOf[Singleton])
  bar.asInstanceOf[Singleton]
  (bar: String).asInstanceOf[Singleton]

  final val baz = "foo"
  assert(baz.isInstanceOf[Singleton])
  assert((baz: String).isInstanceOf[Singleton])
  baz.asInstanceOf[Singleton]
  (baz: String).asInstanceOf[Singleton]

  assert("foo".isInstanceOf[Singleton])
  assert(("foo": String).isInstanceOf[Singleton])
  "foo".asInstanceOf[Singleton]
  ("foo": String).asInstanceOf[Singleton]

  val x = 1
  val y: x.type = x
  assert((y: (x.type with y.type)).isInstanceOf[Singleton])
  assert((y: (x.type with Int)).isInstanceOf[Singleton])
  type A = x.type
  assert((y: A).isInstanceOf[Singleton])
  assert(!(null: String).isInstanceOf[Singleton])
}
