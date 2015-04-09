/*
 * filter: inliner warnings; re-run with
 */
object Test {
  def x = BigInt("10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000")
  def y = BigDecimal("" + (Short.MaxValue + 1) + ".0")
  def y1 = BigDecimal("0.1")
  def y2 = BigDecimal("0.5")

  def l1 = Int.MaxValue.toLong + 1
  def l2 = Int.MinValue.toLong - 1

  def main(args: Array[String]): Unit = {
//    assert(x.isWhole, x)
    assert(!x.isValidDouble, x)
    assert(!x.isValidFloat, x)
    assert(!x.isValidLong, x)
    assert(!x.isValidInt, x)
    assert(!x.isValidChar, x)
    assert(!x.isValidShort, x)
    assert(!x.isValidByte, x)
    assert(y.isWhole, y)
    assert(!y.isValidShort, y)
    assert(y.isValidChar, y)
    assert(y.isValidInt, y)
    assert(y.isDecimalFloat, y)
    assert(y.isDecimalDouble, y)
    assert(y.isValidLong, y)
    assert(!y.isValidByte, y)
    assert(!y1.isWhole)
    assert(!y1.isValidLong, y1)
    assert(y1.isDecimalFloat, y1)
    assert(y1.isDecimalDouble, y1)
    assert(!y1.isExactFloat, y1)
    assert(!y1.isExactDouble, y1)
    assert(!y1.isValidInt, y1)
    assert(!y1.isValidChar, y1)
    assert(!y1.isValidShort, y1)
    assert(!y1.isValidByte, y1)
    assert(!y2.isValidLong, y2)
    assert(y2.isExactFloat, y2)
    assert(y2.isExactDouble, y2)

    assert(!l1.isValidInt && (l1 - 1).isValidInt, l1)
    assert(!l2.isValidInt && (l2 + 1).isValidInt, l2)

    testBigInts()
    testNonWholeDoubles()
    testNaNs()
  }

  def testBigInts() {
    def biExp2(e: Int) = BigInt(1) << e
    def checkBigInt2(bi: BigInt) { checkBigInt(-bi); checkBigInt(bi) }

    val pf = 24
    val pd = 53

    checkBigInt(BigInt(0))
    checkBigInt2(biExp2(0))

    checkBigInt2(biExp2(7) - 1)
    checkBigInt2(biExp2(7))
    checkBigInt2(biExp2(7) + 1)

    checkBigInt2(biExp2(8) - 1)
    checkBigInt2(biExp2(8))
    checkBigInt2(biExp2(8) + 1)

    checkBigInt2(biExp2(15) - 1)
    checkBigInt2(biExp2(15))
    checkBigInt2(biExp2(15) + 1)

    checkBigInt2(biExp2(16) - 1)
    checkBigInt2(biExp2(16))
    checkBigInt2(biExp2(16) + 1)

    checkBigInt2(biExp2(pf) - 1)
    checkBigInt2(biExp2(pf))
    checkBigInt2(biExp2(pf) + 1)
    checkBigInt2(biExp2(pf) + 2)
    checkBigInt2(biExp2(pf) - 2)
    checkBigInt2(biExp2(pf + 1) - 1)
    checkBigInt2(biExp2(pf + 1))
    checkBigInt2(biExp2(pf + 1) + 1)
    checkBigInt2(biExp2(pf + 1) + 2)
    checkBigInt2(biExp2(pf + 1) + 3)
    checkBigInt2(biExp2(pf + 1) + 4)

    checkBigInt2(biExp2(31) - 1)
    checkBigInt2(biExp2(31))
    checkBigInt2(biExp2(31) + 1)

    checkBigInt2(biExp2(32) - 1)
    checkBigInt2(biExp2(32))
    checkBigInt2(biExp2(32) + 1)
    checkBigInt2(biExp2(32) + biExp2(64 - pf))
    checkBigInt2(biExp2(32) + biExp2(64 - pf + 1))

    checkBigInt2(biExp2(pd) - 1)
    checkBigInt2(biExp2(pd))
    checkBigInt2(biExp2(pd) + 1)
    checkBigInt2(biExp2(pd) + 2)
    checkBigInt2(biExp2(pd + 1) - 2)
    checkBigInt2(biExp2(pd + 1) - 1)
    checkBigInt2(biExp2(pd + 1))
    checkBigInt2(biExp2(pd + 1) + 1)
    checkBigInt2(biExp2(pd + 1) + 2)
    checkBigInt2(biExp2(pd + 1) + 3)
    checkBigInt2(biExp2(pd + 1) + 4)

    checkBigInt2(biExp2(63) - 1)
    checkBigInt2(biExp2(63))
    checkBigInt2(biExp2(63) + 1)
    checkBigInt2(biExp2(63) + biExp2(63 - pd))
    checkBigInt2(biExp2(63) + biExp2(63 - pd + 1))
    checkBigInt2(biExp2(63) + biExp2(63 - pf))
    checkBigInt2(biExp2(63) + biExp2(63 - pf + 1))

    checkBigInt2(biExp2(64) - 1)
    checkBigInt2(biExp2(64))
    checkBigInt2(biExp2(64) + 1)
    checkBigInt2(biExp2(64) + biExp2(64 - pd))
    checkBigInt2(biExp2(64) + biExp2(64 - pd + 1))
    checkBigInt2(biExp2(64) + biExp2(64 - pf))
    checkBigInt2(biExp2(64) + biExp2(64 - pf + 1))

    checkBigInt2(biExp2(127))
    checkBigInt2(biExp2(128) - biExp2(128 - pf))
    checkBigInt2(biExp2(128) - biExp2(128 - pf - 1))
    checkBigInt2(biExp2(128))

    checkBigInt2(biExp2(1023))
    checkBigInt2(biExp2(1024) - biExp2(1024 - pd))
    checkBigInt2(biExp2(1024) - biExp2(1024 - pd - 1))
    checkBigInt2(biExp2(1024))
  }

  def testNonWholeDoubles() {
    checkNonWholeDouble(0.5)
    checkNonWholeDouble(-math.E)
    checkNonWholeDouble((1L << 51).toDouble + 0.5)
    checkNonWholeDouble((1L << 23).toDouble + 0.5)
    checkNonWholeDouble(Double.PositiveInfinity)
    checkNonWholeDouble(Double.NegativeInfinity)
  }

  def testNaNs() {
    assert(!Double.NaN.isWhole, Double.NaN)
//    assert(!Double.NaN.isValidDouble, Double.NaN)
//    assert(!Double.NaN.isValidFloat, Double.NaN)
//    assert(!Double.NaN.isValidLong, Double.NaN)
    assert(!Double.NaN.isValidInt, Double.NaN)
    assert(!Double.NaN.isValidChar, Double.NaN)
    assert(!Double.NaN.isValidShort, Double.NaN)
    assert(!Double.NaN.isValidByte, Double.NaN)

    assert(!Float.NaN.isWhole, Float.NaN)
//    assert(!Float.NaN.isValidDouble, Float.NaN)
//    assert(!Float.NaN.isValidFloat, Float.NaN)
//    assert(!Float.NaN.isValidLong, Float.NaN)
    assert(!Float.NaN.isValidInt, Float.NaN)
    assert(!Float.NaN.isValidChar, Float.NaN)
    assert(!Float.NaN.isValidShort, Float.NaN)
    assert(!Float.NaN.isValidByte, Float.NaN)
  }

  def checkNonWholeDouble(d: Double) {
    val f = d.toFloat
    val isFloat = f == d

    if (!d.isInfinity) {
      val bd = BigDecimal(new java.math.BigDecimal(d))
//      assert(!bd.isWhole, bd)
      assert(bd.isExactDouble, bd)
      assert(bd.isExactFloat == isFloat, bd)
      assert(!bd.isValidLong, bd)
      assert(!bd.isValidInt, bd)
      assert(!bd.isValidChar, bd)
      assert(!bd.isValidShort, bd)
      assert(!bd.isValidByte, bd)
    }

    assert(!d.isWhole, d)
//    assert(d.isValidDouble, d)
//    assert(d.isValidFloat == isFloat, d)
//    assert(!d.isValidLong, d)
    assert(!d.isValidInt, d)
    assert(!d.isValidChar, d)
    assert(!d.isValidShort, d)
    assert(!d.isValidByte, d)

    if (isFloat) {
      assert(!f.isWhole, f)
//      assert(f.isValidDouble, f)
//      assert(f.isValidFloat == isFloat, f)
//      assert(!f.isValidLong, f)
      assert(!f.isValidInt, f)
      assert(!f.isValidChar, f)
      assert(!f.isValidShort, f)
      assert(!f.isValidByte, f)
    }
  }

  def checkBigInt(bi: BigInt) {
    val bd = BigDecimal(bi, java.math.MathContext.UNLIMITED)
    val isByte = bi >= Byte.MinValue && bi <= Byte.MaxValue
    val isShort = bi >= Short.MinValue && bi <= Short.MaxValue
    val isChar = bi >= Char.MinValue && bi <= Char.MaxValue
    val isInt = bi >= Int.MinValue && bi <= Int.MaxValue
    val isLong = bi >= Long.MinValue && bi <= Long.MaxValue
    val isFloat = !bi.toFloat.isInfinity && bd.compare(BigDecimal(new java.math.BigDecimal(bi.toFloat))) == 0
    val isDouble = !bi.toDouble.isInfinity && bd.compare(BigDecimal(new java.math.BigDecimal(bi.toDouble))) == 0

    assert(bd.isWhole, bd)
    assert(bd.isBinaryDouble == isDouble, bd)
    assert(bd.isBinaryFloat == isFloat, bd)
    assert(bd.isValidLong == isLong, bd)
    assert(bd.isValidInt == isInt, bd)
    assert(bd.isValidChar == isChar, bd)
    assert(bd.isValidShort == isShort, bd)
    assert(bd.isValidByte == isByte, bd)

//    assert(bi.isWhole, bi)
    assert(bi.isValidDouble == isDouble, bi)
    assert(bi.isValidFloat == isFloat, bi)
    assert(bi.isValidLong == isLong, bi)
    assert(bi.isValidInt == isInt, bi)
    assert(bi.isValidChar == isChar, bi)
    assert(bi.isValidShort == isShort, bi)
    assert(bi.isValidByte == isByte, bi)

    if (isDouble) {
      val d = bi.toDouble
      assert(d.isWhole, d)
//      assert(d.isValidDouble == isDouble, d)
//      assert(d.isValidFloat == isFloat, d)
//      assert(d.isValidLong == isLong, d)
      assert(d.isValidInt == isInt, d)
      assert(d.isValidChar == isChar, d)
      assert(d.isValidShort == isShort, d)
      assert(d.isValidByte == isByte, d)
    }

    if (isFloat) {
      val f = bi.toFloat
      assert(f.isWhole, f)
//      assert(f.isValidDouble == isDouble, f)
//      assert(f.isValidFloat == isFloat, f)
//      assert(f.isValidLong == isLong, f)
      assert(f.isValidInt == isInt, f)
      assert(f.isValidChar == isChar, f)
      assert(f.isValidShort == isShort, f)
      assert(f.isValidByte == isByte, f)
    }

    if (isLong) {
      val l = bi.toLong
      assert(l.isWhole, l)
//      assert(l.isValidDouble == isDouble, l)
//      assert(l.isValidFloat == isFloat, l)
//      assert(l.isValidLong == isLong, l)
      assert(l.isValidInt == isInt, l)
      assert(l.isValidChar == isChar, l)
      assert(l.isValidShort == isShort, l)
      assert(l.isValidByte == isByte, l)
    }

    if (isInt) {
      val i = bi.toInt
      assert(i.isWhole, i)
//      assert(i.isValidDouble == isDouble, i)
//      assert(i.isValidFloat == isFloat, i)
//      assert(i.isValidLong == isLong, i)
      assert(i.isValidInt == isInt, i)
      assert(i.isValidChar == isChar, i)
      assert(i.isValidShort == isShort, i)
      assert(i.isValidByte == isByte, i)
    }

    if (isChar) {
      val c = bi.toChar
      assert(c.isWhole, c)
//      assert(c.isValidDouble == isDouble, c)
//      assert(c.isValidFloat == isFloat, c)
//      assert(c.isValidLong == isLong, c)
      assert(c.isValidInt == isInt, c)
      assert(c.isValidChar == isChar, c)
      assert(c.isValidShort == isShort, c)
      assert(c.isValidByte == isByte, c)
    }

    if (isShort) {
      val s = bi.toShort
      assert(s.isWhole, s)
//      assert(s.isValidDouble == isDouble, s)
//      assert(s.isValidFloat == isFloat, s)
//      assert(s.isValidLong == isLong, s)
      assert(s.isValidInt == isInt, s)
      assert(s.isValidChar == isChar, s)
      assert(s.isValidShort == isShort, s)
      assert(s.isValidByte == isByte, s)
    }

    if (isByte) {
      val b = bi.toByte
      assert(b.isWhole, b)
//      assert(b.isValidDouble == isDouble, b)
//      assert(b.isValidFloat == isFloat, b)
//      assert(b.isValidLong == isLong, b)
      assert(b.isValidInt == isInt, b)
      assert(b.isValidChar == isChar, b)
      assert(b.isValidShort == isShort, b)
      assert(b.isValidByte == isByte, b)
    }
  }
}
