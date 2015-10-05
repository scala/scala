object Extractor {
  def unapply(a: Any): Option[Product2[Int, String]] = Some(new P2(1, "2"))
}
class P2[A, B](val _1: A, val _2: B) extends Product2[A, B] {
  def canEqual(other: Any) = true
  def isP2 = true
}

object Test {
  def main(args: Array[String]): Unit = {
    "" match {
      case Extractor(p) =>
        val pp: Product2[Int, String] = p
    }
    "" match {
      case Extractor(x, y) =>
        val xx: Int = x
        val yy: String = y
    }
  }
}
