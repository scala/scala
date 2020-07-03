package scala.runtime

import org.junit._
import Assert._

import scala.tools.testing.AllocationTest

class DoubleBoxingTest extends SideEffectTest with AllocationTest {
  val nan      = Double.NaN
  val value    = 999D
  val valueInt = 999
  @Test def isNaN: Unit = {
    assertTrue(nonAllocating(nan.isNaN))
    assertFalse(nonAllocating(value.isNaN))
  }
  @Test def isInfinity: Unit = {
    assertFalse(nonAllocating(value.isInfinity))
  }
  @Test def isInfinite: Unit = {
    assertFalse(nonAllocating(value.isInfinite))
  }
  @Test def isNegInfinity: Unit = {
    assertFalse(nonAllocating(value.isNegInfinity))
  }
  @Test def isPosInfinity: Unit = {
    assertFalse(nonAllocating(value.isPosInfinity))
  }
  @Test def float: Unit = {
    assertEquals(value, nonAllocating(value.floatValue()), 0D)
  }
  @Test def double: Unit = {
    assertEquals(value, nonAllocating(value.doubleValue()), 0D)
  }
  @Test def long: Unit = {
    assertEquals(valueInt, nonAllocating(value.longValue()))
  }
  @Test def int: Unit = {
    assertEquals(valueInt, nonAllocating(value.intValue()))
  }
  @Test def short: Unit = {
    assertEquals(valueInt, nonAllocating(value.shortValue()))
  }
  @Test def byte: Unit = {
    assertEquals(-25, nonAllocating(value.byteValue()))
  }
  @Test def str: Unit = {
    val cost = allocationInfo(java.lang.Double.toString(value), "", false)
    assertEquals("999.0", exactAllocates(cost.min)(value.toString()))
  }
  @Test def hash1: Unit = {
    nonAllocating(value.##)
  }
  @Test def hash2: Unit = {
    nonAllocating(value.hashCode())
  }
  @Test def max: Unit = {
    nonAllocating(value max value)
  }
  @Test def min: Unit = {
    nonAllocating(value min value)
  }
  @Test def abs: Unit = {
    nonAllocating(value.abs)
  }
  @Test def signum: Unit = {
    nonAllocating(value.signum)
  }
  @Test def round: Unit = {
    nonAllocating(value.round)
  }
  @Test def ceil: Unit = {
    nonAllocating(value.ceil)
  }
  @Test def floor: Unit = {
    nonAllocating(value.floor)
  }
  @Test def rad: Unit = {
    nonAllocating(value.toRadians)
  }
  @Test def deg: Unit = {
    nonAllocating(value.toDegrees)
  }
  @Test def to: Unit = {
    nonAllocating(value.toByte)
    nonAllocating(value.toShort)
    nonAllocating(value.toInt)
    nonAllocating(value.toLong)
    nonAllocating(value.toFloat)
    nonAllocating(value.toDouble)
  }

  //check that any rewrites don't skip side effects
  @Test def isNaN_SideEffect: Unit = {
    {sideEffect; value}.isNaN()
    checkSideEffected
  }
  @Test def isInfinity_SideEffect: Unit = {
    {sideEffect; value}.isInfinity
    checkSideEffected
  }
  @Test def isInfinite_SideEffect: Unit = {
    {sideEffect; value}.isInfinite()
    checkSideEffected
  }
  @Test def isNegInfinity_SideEffect: Unit = {
    {sideEffect; value}.isNegInfinity
    checkSideEffected
  }
  @Test def isPosInfinity_SideEffect: Unit = {
    {sideEffect; value}.isPosInfinity
    checkSideEffected
  }
  @Test def hash1_SideEffect1(): Unit = {
    {sideEffect; value}.hashCode()
    checkSideEffected
  }
  @Test def hash1_SideEffect2(): Unit = {
    {sideEffect; Predef}.double2Double(value).hashCode()
    checkSideEffected
  }

  @Test def hash2_SideEffect(): Unit = {
    {sideEffect; value}.##
    checkSideEffected
  }

  @Test def str_SideEffect1(): Unit = {
    {sideEffect; value}.toString
    checkSideEffected
  }

  @Test def str_SideEffect2(): Unit = {
    {sideEffect; Predef}.double2Double(value).toString
    checkSideEffected
  }

  @Test def float_SideEffect1(): Unit = {
    {sideEffect; value}.floatValue()
    checkSideEffected
  }
  @Test def float_SideEffect2(): Unit = {
    {sideEffect; Predef}.double2Double(value).floatValue()
    checkSideEffected
  }
  @Test def double_SideEffect1(): Unit = {
    {sideEffect; value}.doubleValue()
    checkSideEffected
  }
  @Test def double_SideEffect2(): Unit = {
    {sideEffect; Predef}.double2Double(value).doubleValue()
    checkSideEffected
  }
  @Test def long_SideEffect1(): Unit = {
    {sideEffect; value}.longValue()
    checkSideEffected
  }
  @Test def long_SideEffect2(): Unit = {
    {sideEffect; Predef}.double2Double(value).longValue()
    checkSideEffected
  }
  @Test def int_SideEffect1(): Unit = {
    {sideEffect; value}.intValue()
    checkSideEffected
  }
  @Test def int_SideEffect2(): Unit = {
    {sideEffect; Predef}.double2Double(value).intValue()
    checkSideEffected
  }
  @Test def short_SideEffect1(): Unit = {
    {sideEffect; value}.shortValue()
    checkSideEffected
  }
  @Test def short_SideEffect2(): Unit = {
    {sideEffect; Predef}.double2Double(value).shortValue()
    checkSideEffected
  }
  @Test def byte_SideEffect1(): Unit = {
    {sideEffect; value}.byteValue()
    checkSideEffected
  }
  @Test def byte_SideEffect2(): Unit = {
    {sideEffect; Predef}.double2Double(value).byteValue()
    checkSideEffected
  }

  @Test def max_SideEffect1: Unit = {
    {sideEffect; value} max value
    checkSideEffected
  }
  @Test def min_SideEffect1: Unit = {
    {sideEffect; value} min value
    checkSideEffected
  }
  @Test def abs_SideEffect1: Unit = {
    {sideEffect; value}.abs
    checkSideEffected
  }
  @Test def signum_SideEffect1: Unit = {
    {sideEffect; value}.signum
    checkSideEffected
  }
  @Test def round_SideEffect1: Unit = {
    {sideEffect; value}.round
    checkSideEffected
  }
  @Test def ceil_SideEffect1: Unit = {
    {sideEffect; value}.ceil
    checkSideEffected
  }
  @Test def floor_SideEffect1: Unit = {
    {sideEffect; value}.floor
    checkSideEffected
  }
  @Test def rad_SideEffect1: Unit = {
    {sideEffect; value}.toRadians
    checkSideEffected
  }
  @Test def deg_SideEffect1: Unit = {
    {sideEffect; value}.toDegrees
    checkSideEffected
  }

}
