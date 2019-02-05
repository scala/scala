trait A {
  def a: Int
}

object Test {
  def f(a: Int) = new {
    //private val b = a
    private[this] val b = a // crashes, sorry scalac
  } with A {
    def a = b
  }

  def main(args: Array[String]): Unit = {
    println(f(7).a)
  }
}
