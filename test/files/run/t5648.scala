case class C(val s: Int*)

object Test {
  def main(args: Array[String]): Unit = {
    println(new C(1, 3, 7) == new C(1, 3, 7))
    println(new C(1, 3, 7) == C(1, 3, 7))
    println(C(1, 3, 7) == new C(1, 3, 7))
    println(C(1, 3, 7) == C(1, 3, 7))
  }
}
