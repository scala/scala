object s {
  def f(x: Cloneable) = ()
  def g(x: java.io.Serializable) = ()

  def main(args: Array[String]): Unit = {
    f(args)
    g(args)
  }
}