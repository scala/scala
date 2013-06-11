class Foo(val a: Int) extends AnyVal {
  def foo = { {case x => x + a}: PartialFunction[Int, Int]}

  def bar = (new {}).toString
}

object Test extends App {
  val x = new Foo(1).foo.apply(2)
  assert(x == 3, x)
  val s = new Foo(1).bar
  assert(s.nonEmpty, s)
}
