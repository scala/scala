object Test {
  def f[T](xs: T*): T = xs.head
  def g[T] = f[T] _

  def main(args: Array[String]): Unit = {
    println(g("hello" +: args))
  }
}
