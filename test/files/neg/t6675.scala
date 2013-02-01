object X {
  def unapply(s: String): Option[(Int,Int,Int)] = Some((1,2,3))
}

object Y {
  def unapplySeq(s: String): Option[Seq[(Int,Int,Int)]] = Some(Seq((1,2,3)))
}

object Test {
  "" match { case X(b) => b } // should warn under -Xlint. Not an error because of SI-6111

  "" match { case Y(b) => b } // no warning
}
