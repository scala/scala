class M[_]
trait T {
  def foo(m: M[_ >: String]) = 42
}

object Test {
  def t: T = new T {
    def foo(m: M[_ >: Any]) = 0 // Expected: "same type after erasure"
  }
  def main(args: Array[String]): Unit = {
    val m: M[String] = null
    t.foo(m) // VeriyError: Duplicate method name&signature
  }
}
