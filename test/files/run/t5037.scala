object Test {
  def main(args: Array[String]) {
    val t = new Test
    t.inner.foo()
  }
}

class Test {
  class Inner {
    def foo() {
      println(bar)
      bar = false
      println(bar)
    }
  }
  val inner = new Inner
  private[this] final var bar = true
}
