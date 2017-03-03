object Test {
  def main(args: Array[String]): Unit = {
    intShiftLeftLongConstantFolded()
    intShiftLeftLongAtRuntime()
    intShiftLogicalRightLongConstantFolded()
    intShiftLogicalRightLongAtRuntime()
    intShiftArithmeticRightLongConstantFolded()
    intShiftArithmeticRightLongAtRuntime()
  }

  def intShiftLeftLongConstantFolded(): Unit = {
    assert(0x01030507 << 36L == 271601776)
    val r = 0x01030507 << 36L
    assert(r == 271601776)
  }

  def intShiftLeftLongAtRuntime(): Unit = {
    var x: Int = 0x01030507
    var y: Long = 36L
    assert(x << y == 271601776)
    val r = x << y
    assert(r == 271601776)
  }

  def intShiftLogicalRightLongConstantFolded(): Unit = {
    assert(0x90503010 >>> 36L == 151323393)
    val r = 0x90503010 >>> 36L
    assert(r == 151323393)
  }

  def intShiftLogicalRightLongAtRuntime(): Unit = {
    var x: Int = 0x90503010
    var y: Long = 36L
    assert(x >>> y == 151323393)
    val r = x >>> y
    assert(r == 151323393)
  }

  def intShiftArithmeticRightLongConstantFolded(): Unit = {
    assert(0x90503010 >> 36L == -117112063)
    val r = 0x90503010 >> 36L
    assert(r == -117112063)
  }

  def intShiftArithmeticRightLongAtRuntime(): Unit = {
    var x: Int = 0x90503010
    var y: Long = 36L
    assert(x >> y == -117112063)
    val r = x >> y
    assert(r == -117112063)
  }
}
