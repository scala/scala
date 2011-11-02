import scala.collection.immutable.{ Range, NumericRange }

object Test {
  def rangeForeach(range : Range) = {
    val buffer = new scala.collection.mutable.ListBuffer[Int];
    range.foreach(buffer += _);
    assert(buffer.toList == range.iterator.toList, buffer.toList+"/"+range.iterator.toList)
  }
  
  def boundaryTests() = {
    // #4321
    assert((Int.MinValue to Int.MaxValue by Int.MaxValue).size == 3)
    // #4308
    val caught = (
      try   { (Long.MinValue to Long.MaxValue).sum ; false }
      catch { case _: IllegalArgumentException => true }
    )
    assert(caught)
  }
  
  case class GR[T](val x: T)(implicit val num: Integral[T]) {
    import num._
    
    def negated = GR[T](-x)
    
    def gr1 = NumericRange(x, x, x)
    def gr2 = NumericRange.inclusive(x, x, x)
    def gr3 = NumericRange(x, x * fromInt(10), x)
    def gr4 = NumericRange.inclusive(x, x * fromInt(10), x)
    def gr5 = gr3.toList ::: negated.gr3.toList
    
    def check = {
      assert(gr1.isEmpty && !gr2.isEmpty)
      assert(gr3.size == 9 && gr4.size == 10)      
      assert(gr5.sum == num.zero, gr5.toString)
      assert(!(gr3 contains (x * fromInt(10))))
      assert((gr4 contains (x * fromInt(10))))
    }
  }
  
  def main(args: Array[String]): Unit = {
    implicit val imp1 = Numeric.BigDecimalAsIfIntegral
    implicit val imp2 = Numeric.DoubleAsIfIntegral
    
    val _grs = List[GR[_]](
      GR(BigDecimal(5.0)),
      GR(BigInt(5)),
      GR(5L),
      GR(5.0d),
      GR(2.toByte)
    )
    val grs = _grs ::: (_grs map (_.negated))
    grs foreach (_.check)
    
    assert(NumericRange(1, 10, 1) sameElements (1 until 10))
    assert(NumericRange.inclusive(1, 10, 1) sameElements (1 to 10))
    assert(NumericRange.inclusive(1, 100, 3) sameElements (1 to 100 by 3))
    
    // #2518
    assert((3L to 7 by 2) sameElements List(3L, 5L, 7L))
    
    rangeForeach(1 to 10);
    rangeForeach(1 until 10);
    rangeForeach(10 to 1 by -1);
    rangeForeach(10 until 1 by -1);
    rangeForeach(10 to 1 by -3);
    rangeForeach(10 until 1 by -3);
    
    // living on the edges
    boundaryTests()
  }
}
