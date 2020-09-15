object Test {
  case object Bob { override def equals(other: Any) = true }
  def f(x: Any) = x match { case Bob => Bob case x => throw new MatchError(x) }

  def main(args: Array[String]): Unit = {
    assert(f(Bob) eq Bob)
    assert(f(0) eq Bob)
    assert(f(Nil) eq Bob)
  }
}
