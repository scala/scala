case class T1(source: String)

object T1 {
  implicit def T1FromT2(implicit t2: T2) = new T1(t2.source)
}

case class T2(source: String)

object A {
  def requireT1(implicit t1: T1) = t1

  object B1 {
    implicit val t2_b1 = new T2("from B1")
    requireT1
  }

  object B2 {
    def t1 = {
      implicit val t2_b2 = new T2("from B2")
      // Implicits.cacheResult returns T1.T1FromT2(t2_b1) here, which is bogus. Even though T1.T1FromT2 was found
      // outside of the scope of A.B1, this implicit expression should _not_ be cached, as it includes the bound
      // variable t2_b1 from this scope.
      requireT1
    }
  }
}

object Test {
  def main(args: Array[String]) {
    assert(A.B2.t1.source == "from B2")
  }
}