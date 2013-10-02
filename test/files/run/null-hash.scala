object Test {
  def f1 = List(5, 10, null: String).##
  def f2(x: Any) = x.##
  def f3 = ((55, "abc", null: List[Int])).##

  def main(args: Array[String]): Unit = {
    f1
    f2(null)
    f2(null: String)
    f3
    null.##
    (null: Any).##
    (null: String).##
  }
}
