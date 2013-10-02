object Test {
  def f1(x: Any) = x match {
    case Seq(1, 2, 3) => "s3"
    case Seq(4, 5) => "s2"
    case Seq(7) => "s1"
    case Nil => "s0"
    case Seq(_, _, _, _, _, x: String) => "ss6"
    case _ => "d"
  }

  def f2(x: Any) = x match {
    case Seq("a", "b", _*)  => "s2"
    case Seq(1, _*)         => "s1"
    case Seq(5, 6, 7, _*)   => "s3"
    case _                  => "d"
  }

  def main(args: Array[String]): Unit = {
    val xs1 = List(
      List(1,2,3),
      List(4,5),
      Vector(7),
      Seq(),
      Seq(1, 2, 3, 4, 5, "abcd"),
      "abc"
    ) map f1

    xs1 foreach println

    val xs2 = List(
      Seq(5, 6, 7),
      Seq(5, 6, 7, 8, 9),
      Seq("a"),
      Seq(1, 6, 7),
      List(5, 6, 7),
      Nil,
      5
    ) map f2

    xs2 foreach println
  }
}
