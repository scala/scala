object Test {
  def main(args: Array[String]): Unit = {
    val xs = Seq(1,2,3).view.groupBy(identity)
    assert(xs.size == 3)
  }
}
