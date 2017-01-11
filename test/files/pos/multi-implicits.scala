package test

object Test {
  // A couple of type classes with type members ...
  trait Foo[T] {
    type A
  }

  object Foo {
    implicit val fooIS = new Foo[Int] { type A = String }
  }

  trait Bar[T] {
    type B
    val value: B
  }

  object Bar {
    implicit val barSB = new Bar[String] {
      type B = Boolean
      val value = true
    }
  }

  def run[T](t: T)(implicit foo: Foo[T])(bar: Bar[foo.A]): bar.B = bar.value

  val value = run(23)
  assert(value: Boolean)
}
