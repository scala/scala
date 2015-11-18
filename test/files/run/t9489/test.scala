class T {
  def f(a: A) = g(a.b) // was: "found Int, required B"
  def g(b: => B) = null
}

object Test extends T {
  def main(args: Array[String]): Unit = {
    f(new A)
  }
}
