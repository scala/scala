object Test {
  def main(args: Array[String]): Unit = {
    val a = Array.ofDim[Int](2,2)
    test(a)
  }
  def test[A](t: Array[Array[A]]): Unit = {
    val tmp = t(0)
    t(1) = tmp
  }
}
