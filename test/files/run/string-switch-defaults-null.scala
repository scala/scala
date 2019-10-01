import annotation.switch

object Test {
  def test(s: String): Int = {
    (s : @switch) match {
      case "1" => 0
      case null => -1
      case _ => s.toInt
    }
  }

  def main(args: Array[String]): Unit = {
    println(test("2"))
    println(test(null))
  }
}
