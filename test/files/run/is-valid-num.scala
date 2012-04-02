object Test {
  def x = BigInt("10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000")
  def y = BigDecimal("" + (Short.MaxValue + 1) + ".0")
  def y1 = BigDecimal("0.1")
  def y2 = BigDecimal("0.5")
  
  def l1 = Int.MaxValue.toLong + 1
  def l2 = Int.MinValue.toLong - 1

  def main(args: Array[String]): Unit = {
    assert(!x.isValidDouble, x)
    assert(!x.isValidFloat, x)
    assert(!x.isValidLong, x)
    assert(!x.isValidInt, x)
    assert(!x.isValidChar, x)
    assert(!x.isValidByte, x)
    assert(!y.isValidShort, y)
    assert(y.isValidChar, y)
    assert(y.isValidInt, y)
    assert(y.isValidFloat, y)
    assert(y.isValidDouble, y)
    assert(!y1.isValidLong, y1)
    assert(!y1.isValidFloat, y1)
    assert(!y1.isValidDouble, y1)
    assert(!y2.isValidLong, y2)
    assert(y2.isValidFloat, y2)
    assert(y2.isValidDouble, y2)

    testBigIntIsFloat()
    testBigIntIsDouble()

    assert(!l1.isValidInt && (l1 - 1).isValidInt, l1)
    assert(!l2.isValidInt && (l2 + 1).isValidInt, l2)
  }

  def biExp2(e: Int) = BigInt(1) << e

  def testBigIntIsFloat() {
    val prec = 24
    def checkFloatT(x: BigInt) = {
      assert(x.isValidFloat, x)
      assert((-x).isValidFloat, -x)
    }
    def checkFloatF(x: BigInt) = {
      assert(!x.isValidFloat, x)
      assert(!(-x).isValidFloat, -x)
    } 
    checkFloatT(biExp2(prec) - 1)
    checkFloatT(biExp2(prec))
    checkFloatF(biExp2(prec) + 1)
    checkFloatT(biExp2(prec) + 2)
    checkFloatT(biExp2(prec) - 2)
    checkFloatF(biExp2(prec + 1) - 1)
    checkFloatT(biExp2(prec + 1))
    checkFloatF(biExp2(prec + 1) + 1)
    checkFloatF(biExp2(prec + 1) + 2)
    checkFloatF(biExp2(prec + 1) + 3)
    checkFloatT(biExp2(prec + 1) + 4)
    checkFloatT(biExp2(64))
    checkFloatF(biExp2(64) + biExp2(64 - prec))
    checkFloatT(biExp2(64) + biExp2(64 - prec + 1))
    checkFloatT(biExp2(127))
    checkFloatT(biExp2(128) - biExp2(128 - prec))
    checkFloatF(biExp2(128) - biExp2(128 - prec - 1))
    checkFloatF(biExp2(128))
  }

  def testBigIntIsDouble() {
    val prec = 53
    def checkDoubleT(x: BigInt) = {
      assert(x.isValidDouble, x)
      assert((-x).isValidDouble, -x)
    }
    def checkDoubleF(x: BigInt) = {
      assert(!x.isValidDouble, x)
      assert(!(-x).isValidDouble, -x)
    }
    checkDoubleT(biExp2(prec) - 1)
    checkDoubleT(biExp2(prec))
    checkDoubleF(biExp2(prec) + 1)
    checkDoubleT(biExp2(prec) + 2)
    checkDoubleT(biExp2(prec + 1) - 2)
    checkDoubleF(biExp2(prec + 1) - 1)
    checkDoubleT(biExp2(prec + 1))
    checkDoubleF(biExp2(prec + 1) + 1)
    checkDoubleF(biExp2(prec + 1) + 2)
    checkDoubleF(biExp2(prec + 1) + 3)
    checkDoubleT(biExp2(prec + 1) + 4)
    checkDoubleT(biExp2(64))
    checkDoubleF(biExp2(64) + biExp2(64 - prec))
    checkDoubleT(biExp2(64) + biExp2(64 - prec + 1))
    checkDoubleT(biExp2(1023))
    checkDoubleT(biExp2(1024) - biExp2(1024 - prec))
    checkDoubleF(biExp2(1024) - biExp2(1024 - prec - 1))
    checkDoubleF(biExp2(1024))
  }
}
