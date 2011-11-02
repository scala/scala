object Test {
  def x = BigInt("10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000")
  def y = BigDecimal("" + (Short.MaxValue + 1) + ".0")
  
  def l1 = Int.MaxValue.toLong + 1
  def l2 = Int.MinValue.toLong - 1

  def main(args: Array[String]): Unit = {
    assert(!x.isValidLong, x)
    assert(!x.isValidInt, x)
    assert(!x.isValidChar, x)
    assert(!x.isValidByte, x)
    assert(!y.isValidShort, y)
    assert(y.isValidChar, y)
    assert(y.isValidInt, y)

    assert(!l1.isValidInt && (l1 - 1).isValidInt, l1)
    assert(!l2.isValidInt && (l2 + 1).isValidInt, l2)
  }
}
