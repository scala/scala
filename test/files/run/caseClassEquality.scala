object Test {
  abstract class A1
  case class C1(x: Int) extends A1
  class C2(x: Int) extends C1(x) {
    override def productPrefix = "Shazbot!"
  }
  class C3(x: Int) extends C1(x) {
    override def canEqual(other: Any) = other.isInstanceOf[C3]
    override def equals(other: Any) = other match {
      case ob: C3 => x == ob.x
      case _      => false
    }
  }
  
  case class CS1(xs: Any*)
  class CS2(xs: Seq[_]*) extends CS1(xs: _*)
  class CS3(xs: IndexedSeq[Int]*) extends CS2(xs: _*)
  
  case class H1(x: Int, y: Double)
  class H2(x: Double, y: Int) extends H1(y, x)
  
  def main(args: Array[String]): Unit = {
    assert(C1(5) == new C2(5))
    assert(new C2(5) == C1(5))
    assert(C1(5).hashCode == new C2(5).hashCode)
    assert(new C2(5).hashCode == C1(5).hashCode)
  
    assert(C1(5) != new C3(5))
    assert(new C3(5) != C1(5))
    
    assert(CS1(List(1d,2d), Seq[Float](3f, 4f)) == new CS3(IndexedSeq(1,2), IndexedSeq(3, 4)))
    
    assert(H1(5, 10d) == new H2(10d, 5))
    assert(H1(5, 10d).hashCode == new H2(10d, 5).hashCode)
  }
}
