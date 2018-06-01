object Foo {
  // Currently works
  def substring(s: String, start: Int = 0)(end: Int = s.length) =
    s.substring(start, end)

  // SIP proposes to make these invocations valid
  def foo(a: Int, b: Int = a) = (a, b)
  def foo2(a: Int, b: Int = a, c: Int = a + b) = (a, b, c)
  def foo3(a: Int, b: Int = a)(c: Int = a + b) = (a, b, c)
  def foo4(a: Int, b: Int = a, c: Int = a + b) = (a, b, c)
  def foo5(a: Int = 10, b: Int = a, c: Int = a + b) = (a, b, c)
  def foo6(a: String, b: Int = a.toInt, c: Int = a.toInt + b) = (a.toInt, b, c)

  def fooHK[T](a: T, b: T = a) = (a, b)
  def fooHK2[T](a: T, b: T = a)(c: T = a) = (a, b, c)
  def bind(name: String, boundType: String, value: Any, modifiers: List[String] = Nil): List[String] = modifiers
}

object Test extends App {
  Foo.substring("")()

  val call0 = Foo.foo(1)
  val call02 = Foo.foo(1, 1)
  assert(call0 == call02)

  val a = 1
  val call1 = Foo.foo(a)
  val call12 = Foo.foo(a, 1)
  assert(call1 == call12)

  val call2 = Foo.foo2(1)
  val call22 = Foo.foo2(1, 1)
  val call23 = Foo.foo2(1, 1, 2)
  assert(call2 == call22)
  assert(call2 == call23)

  val call3 = Foo.foo2(a)
  val call32 = Foo.foo2(a, 1)
  val call33 = Foo.foo2(a, 1, 2)
  assert(call3 == call32)
  assert(call3 == call33)

  val call4 = Foo.foo3(1)()
  val call42 = Foo.foo3(1, 1)()
  val call43 = Foo.foo3(1, 1)(2)
  assert(call4 == call42)
  assert(call4 == call43)

  val call5 = Foo.foo3(a)()
  val call52 = Foo.foo3(a, 1)()
  val call53 = Foo.foo3(a, 1)(2)
  assert(call5 == call52)
  assert(call5 == call53)

  val bar = 4
  val call6 = Foo.foo4(a, c = bar)
  assert(call6 == (a, a, bar), s"$call6 != ($a,$a,$bar)")

  val call7 = Foo.foo4(a, b = bar)
  assert(call7 == (a, bar, 5), s"$call7 != ($a,$bar,5)")

  val call8 = Foo.foo5()
  val call82 = Foo.foo5(c = 3)
  assert(call82 == (10, 10, 3), s"$call82 != (10, 10, 3)")

  val call9 = Foo.foo6("1")
  assert(call9._1.intValue() == 1, s"${call9._1} != 1")
  assert(call9._2.intValue() == 1, s"${call9._2} != 1")
  assert(call9._3.intValue() == 2, s"${call9._3} != 2")

  val asdf = "asdfasdf"
  val call100 = Foo.fooHK(asdf)
  assert(call100 == (asdf,asdf), s"$call100 != ($asdf,$asdf)")

  val call110 = Foo.fooHK2(asdf)()
  assert(call110 == (asdf,asdf,asdf), s"$call110 != ($asdf,$asdf,$asdf)")

  val value = ""
  val call120 = Foo.bind("", value.asInstanceOf[AnyRef].getClass.getName, value)
  assert(call120 == Nil, s"$call120 != Nil")
}
