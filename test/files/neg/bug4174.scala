class C

object Test {
  def foo(c: C) = 0

  def main(args: Array[String]): Unit = {
    foo(new C { override def bar = 1 })
  }
}
