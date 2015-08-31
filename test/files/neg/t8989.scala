class A extends Product1[Int] {
    def _1 = 1
    def isEmpty = false // used by scalac
    def isDefined = !isEmpty // used by dotty
    def canEqual(a: Any) = true
}

object d{
  def unapply(a: Any) = new A
  val p: Any = ???
  val f = p match {case d(1) => true; case _ => false}
}


