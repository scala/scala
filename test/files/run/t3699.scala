object Test {
  def act: Int => Int = {
    case _ =>
      lazy val (a, b) = (3,9)
      a
      b
  }
  def main(args: Array[String]) = {
    assert(act(1) == 9)
  }
}