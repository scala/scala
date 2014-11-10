trait Consumer[T] {
  def consume(x: T): Unit
}

object Test {
  def foo(x: String): Unit = ???
  def foo(): Unit = ???
  val f: Consumer[_ >: String] = foo
}