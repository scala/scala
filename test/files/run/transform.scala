object Test {
  val x = (1 to 10).toBuffer

  def main(args: Array[String]): Unit = {
    x transform (_ * 2)
    assert(x.sum == (1 to 10).sum * 2)
  }
}
