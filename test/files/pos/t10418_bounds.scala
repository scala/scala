class Test {
  def foo(c: java.util.Collection[String]): Unit = {
    c.removeIf(x => true)
  }
}
