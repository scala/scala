class Test {
  def test[A](list: List[A]) = list match {
    case Seq(x, y) => "xy"
    case Seq(x) => "x"
    case _ => "something else"
  }
}