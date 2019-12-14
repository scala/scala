object Test {
  trait Foo[A]
  trait Bar[A]
  implicit def fooInt: Foo[Int] = ???
  implicit def fooLong: Foo[Long] = ???
  def bar[A](implicit foo: Foo[A]): Bar[A] = ???
  val fooString: Foo[String] = implicitly
  val barString: Bar[String] = bar
}
