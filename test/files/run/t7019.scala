final class Foo(val i: Int) extends AnyVal {
  def foo() = go(i)
  private[this] def go(i: Int) = i * 2
}

object Test {
  def main(args: Array[String]): Unit = {
    assert(new Foo(1).foo() == 2)
  }
}
