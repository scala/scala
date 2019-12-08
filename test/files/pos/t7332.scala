object Test {
  trait Foo[A]
  implicit val c: Foo[Int] = ???
  implicit val d: Foo[String] = ???
  def bar[A: Foo]: A = ???
  bar: Int
}
