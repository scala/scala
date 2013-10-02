object Test {
  val m = Map(1 -> 1, 2 -> 2)
  val mv = (m mapValues identity) - 1

  def main(args: Array[String]): Unit = {
    assert(mv.size == 1)
  }
}
