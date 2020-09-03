package scala.runtime

import org.junit.Assert._
import org.junit.Test

import scala.tools.testing.AllocationTest

class LongBoxingTest extends SideEffectTest with AllocationTest {
  val value = 99999999999999L

  @Test def hash1(): Unit = {
    nonAllocating(value.hashCode())
  }

  @Test def hash2(): Unit = {
    nonAllocating(value.##)
  }

  @Test def float: Unit = {
    nonAllocating(value.floatValue())
  }
  @Test def double: Unit = {
    nonAllocating(value.doubleValue())
  }
  @Test def long: Unit = {
    nonAllocating(value.longValue())
  }
  @Test def int: Unit = {
    nonAllocating(value.intValue())
  }
  @Test def short: Unit = {
    nonAllocating(value.shortValue())
  }
  @Test def byte: Unit = {
    nonAllocating(value.byteValue())
  }

  @Test def str(): Unit = {
    val cost = allocationInfo(java.lang.Long.toString(value), "", false)
    assertEquals("99999999999999", exactAllocates(cost.min)(value.toString()))
  }
  //check that any rewrites don't skip side effects
  @Test def hash1_SideEffect1(): Unit = {
    {sideEffect; value}.hashCode()
    checkSideEffected
  }
  @Test def hash1_SideEffect2(): Unit = {
    {sideEffect; Predef}.long2Long(value).hashCode()
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
    {sideEffect; Predef}.long2Long(value).toString
    checkSideEffected
  }

  @Test def float_SideEffect1(): Unit = {
    {sideEffect; value}.floatValue()
    checkSideEffected
  }
  @Test def float_SideEffect2(): Unit = {
    {sideEffect; Predef}.long2Long(value).floatValue()
    checkSideEffected
  }
  @Test def double_SideEffect1(): Unit = {
    {sideEffect; value}.doubleValue()
    checkSideEffected
  }
  @Test def double_SideEffect2(): Unit = {
    {sideEffect; Predef}.long2Long(value).doubleValue()
    checkSideEffected
  }
  @Test def long_SideEffect1(): Unit = {
    {sideEffect; value}.longValue()
    checkSideEffected
  }
  @Test def long_SideEffect2(): Unit = {
    {sideEffect; Predef}.long2Long(value).longValue()
    checkSideEffected
  }
  @Test def int_SideEffect1(): Unit = {
    {sideEffect; value}.intValue()
    checkSideEffected
  }
  @Test def int_SideEffect2(): Unit = {
    {sideEffect; Predef}.long2Long(value).intValue()
    checkSideEffected
  }
  @Test def short_SideEffect1(): Unit = {
    {sideEffect; value}.shortValue()
    checkSideEffected
  }
  @Test def short_SideEffect2(): Unit = {
    {sideEffect; Predef}.long2Long(value).shortValue()
    checkSideEffected
  }
  @Test def byte_SideEffect1(): Unit = {
    {sideEffect; value}.byteValue()
    checkSideEffected
  }
  @Test def byte_SideEffect2(): Unit = {
    {sideEffect; Predef}.long2Long(value).byteValue()
    checkSideEffected
  }

}
