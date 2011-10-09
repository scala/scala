object Test {
  def f[T](xs: T*): Unit = ()
  def g[T] = f[T] _

  def main(args: Array[String]): Unit = {
    g(1, 2)
  }
}
