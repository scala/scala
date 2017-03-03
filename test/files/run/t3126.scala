object Test {
  case class C(x: Int)
  val v: Some[Int] = null

  def main(args: Array[String]): Unit = {
    try C.unapply(null) catch { case _: MatchError => }
    try ((v: @unchecked) match { case Some(1) => }) catch { case _: MatchError => }
  }
}
