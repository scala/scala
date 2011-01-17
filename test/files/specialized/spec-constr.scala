object Test {
  class E[@specialized(Int) A](var f: A => Boolean) {
    def this() = this(null)

    println("hello?")
    if (f == null) f = { _ => false }
  }

  def main(args: Array[String]) {
    new E[Int]
    println("goodbye")
    println(runtime.BoxesRunTime.integerBoxCount)
  }
}

