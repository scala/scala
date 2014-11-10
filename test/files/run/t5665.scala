object O {
  trait T {
    private[this] val c: Int = 42
    def f =
      { x: Int => c }
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    assert(new O.T{}.f(0) == 42)
  }
}
