object Foo1 {
  def apply[T](a: Int = 0): Nothing = sys.error("")
  def apply[T](z: String = ""): Nothing = sys.error("")
}

object Test {
  // Triggered a cycle in Typers#adapt
  def test[T]: Xxxx = Foo1[T]
}
