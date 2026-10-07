package scala.lang.primitives

import org.junit.Test

@deprecated("Tests int shift by long", since="2.13")
class Shifts {

  @Test
  def intShiftLeftLongConstantFolded(): Unit = {
    assert(0x01030507 << 36L == 271601776)
    val r = 0x01030507 << 36L
    assert(r == 271601776)
  }

  @Test
  def intShiftLeftLongAtRuntime(): Unit = {
    val x: Int = 0x01030507
    val y: Long = 36L
    assert(x << y == 271601776)
    val r = x << y
    assert(r == 271601776)
  }

  @Test
  def intShiftLogicalRightLongConstantFolded(): Unit = {
    assert(0x90503010 >>> 36L == 151323393)
    val r = 0x90503010 >>> 36L
    assert(r == 151323393)
  }

  @Test
  def intShiftLogicalRightLongAtRuntime(): Unit = {
    val x: Int = 0x90503010
    val y: Long = 36L
    assert(x >>> y == 151323393)
    val r = x >>> y
    assert(r == 151323393)
  }

  @Test
  def intShiftArithmeticRightLongConstantFolded(): Unit = {
    assert(0x90503010 >> 36L == -117112063)
    val r = 0x90503010 >> 36L
    assert(r == -117112063)
  }

  @Test
  def intShiftArithmeticRightLongAtRuntime(): Unit = {
    val x: Int = 0x90503010
    val y: Long = 36L
    assert(x >> y == -117112063)
    val r = x >> y
    assert(r == -117112063)
  }
}
