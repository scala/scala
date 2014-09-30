case class Foo(i: Int, c: Char)

object Bar {
  def unapply(foo: Foo): Option[(Int, Char)] = Some((foo.i, foo.c))
}

object Test extends App {
  println(Macros.typecheck("val Foo(x, y, z) = Foo(1, 'a')"))
  println(Macros.typecheck("val Bar(x, y, z) = Foo(1, 'a')"))
}