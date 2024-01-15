object Test {
  def main(args: Array[String]): Unit = assert(f == 42)
  def f = {
    object Foo {
      val bar = Foo(42)
    }
    case class Foo(i: Int)

    Foo.bar.i
  }
}
