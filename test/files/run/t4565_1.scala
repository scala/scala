trait T {
  def foo = {
    object A
    def a(x: Any) = x == A
    a(A)
  }
}

object Test {
  def main(args: Array[String]) {
    val t = new T{}
    println(t.foo)
  }
}