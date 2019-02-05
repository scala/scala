trait Foo {
  type Out
  def out: Out
}

object Foo {
  type Aux[Out0] = Foo { type Out = Out0 }

  implicit val fooInt: Aux[Int] = new Foo { type Out = Int ; def out = 23 }
}

object Test {
  def bar[T](t: T)(implicit foo: => Foo.Aux[T]): T = foo.out

  val i = bar(13)
  i: Int
}
