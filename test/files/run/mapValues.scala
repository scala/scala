object Test {
  val m = Map(1 -> 1, 2 -> 2)
  val mv = (m.view mapValues identity).toMap - 1

  def main(args: Array[String]): Unit = {
    assert(mv.size == 1)
  }
}
