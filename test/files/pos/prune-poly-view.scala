object Test {
  class Foo[T]
  object Foo {
    implicit def fromT[T](t: T): Foo[T] = ???
  }

  def bar[T](foo: Foo[T]) = ???

  bar[Double](foo = 0)
}

object Test2 {
  class Foo[T]
  object Foo {
    implicit def fromT[T](t: T): Foo[T] = ???
  }

  def bar[T](foo: Foo[T]) = ???

  class C
  object O extends C

  bar[C](foo = O)
}

object Test3 {
  implicit def toOption[T](v: T): Option[T] = Option(v)
  val a: Int = 123
  val b: Option[Long] = a // Works under 2.12.6 but not with the implicit-poly-prune-2.12.x PR
}
