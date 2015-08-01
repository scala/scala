package scala.collection.immutable

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

@RunWith(classOf[JUnit4])
class RangeUnitaryTest {
  val powerOf2Range = Int.MinValue to Int.MaxValue by 65536*128

  @Test
  def doesRangeSupportBigPowerOf2Steps(): Unit = {
    assert(powerOf2Range.length == 512, s"${powerOf2Range.length} != 512")
  }

  @Test
  def doesRangeGenerateCorrectVectorOnPowerOf2Steps(): Unit = {
    assert(powerOf2Range.toVector.length == 512, s"${powerOf2Range.toVector.length} != 512")
  }

  @Test
  def doesRangeGenerateCorrectListOnPowerOf2Steps(): Unit = {
    assert(powerOf2Range.toList.length == 512, s"${powerOf2Range.toList.length} != 512")
  }

  @Test
  def doesVectorEqualArrayOnPowerOf2Steps(): Unit = {
    assert(powerOf2Range.toArray.deep == powerOf2Range.toVector.toArray.deep, "vectors don't equal")
  }

  @Test
  def doesListEqualArrayOnPowerOf2Steps(): Unit = {
    assert(powerOf2Range.toArray.deep == powerOf2Range.toList.toArray.deep, "lists don't equal")
  }
}
