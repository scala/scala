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
    // #7432
    val noElemAtMin = (
      try   { (10 until 10).min ; false }
      catch { case _: NoSuchElementException => true }
    )
    assert(noElemAtMin)
    val noElemAtMax = (
      try   { (10 until 10).max ; false }
      catch { case _: NoSuchElementException => true }
    )
    assert(noElemAtMax)
  }

  case class GR[T](val x: T)(implicit val num: Integral[T]) {
    import num._

    def negated = GR[T](-x)

    def gr1 = NumericRange(x, x, x)
    def gr2 = NumericRange.inclusive(x, x, x)
    def gr3 = NumericRange(x, x * fromInt(4), x * fromInt(2))  // SI-9348
    def gr4 = NumericRange(x, x * fromInt(-2), x * fromInt(-2))
    def gr5 = NumericRange(x, x * fromInt(10), x)
    def gr6 = NumericRange.inclusive(x, x * fromInt(10), x)
    def gr7 = gr3.toList ::: negated.gr3.toList

    def check = {
      assert(gr1.isEmpty && !gr2.isEmpty)
      assert(gr3.size == 2 && gr4.size == 2)
      assert(gr5.size == 9 && gr6.size == 10)
      assert(gr7.sum == num.zero, gr7.toString)
      assert(!(gr5 contains (x * fromInt(10))))
      assert(gr6 contains (x * fromInt(10)))
    }
  }

  def main(args: Array[String]): Unit = {
    implicit val imp1 = Numeric.BigDecimalAsIfIntegral
    implicit val imp2 = Numeric.DoubleAsIfIntegral

    val _grs = List[GR[_]](
      GR(BigDecimal(5.0)),
      GR(BigDecimal(0.25)),  // SI-9348
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
