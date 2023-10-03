package scala.runtime

import org.junit.Assert._
import org.junit.Test

import scala.annotation.nowarn
import scala.tools.testkit.AllocationTest

@nowarn("cat=w-flag-value-discard")
class ShortBoxingTest extends SideEffectTest with AllocationTest {
  val value: Short = 999.toShort

  @Test def hash1(): Unit = {
    nonAllocating(value.hashCode())
  }

  @Test def hash2(): Unit = {
    nonAllocating(value.##)
  }

  @Test def float(): Unit = {
    nonAllocating(value.floatValue())
  }
  @Test def double(): Unit = {
    nonAllocating(value.doubleValue())
  }
  @Test def long(): Unit = {
    nonAllocating(value.longValue())
  }
  @Test def int(): Unit = {
    nonAllocating(value.intValue())
  }
  @Test def short(): Unit = {
    nonAllocating(value.shortValue())
  }
  @Test def byte(): Unit = {
    nonAllocating(value.byteValue())
  }

  @Test def str(): Unit = {
    val cost = allocationInfo(java.lang.Short.toString(value))
    assertEquals("999", exactAllocates(cost.min)(value.toString()))
  }
  //check that any rewrites don't skip side effects
  @Test def hash1_SideEffect1(): Unit = {
    {sideEffect(); value}.hashCode()
    checkSideEffected()
  }
  @Test def hash1_SideEffect2(): Unit = {
    {sideEffect(); Predef}.short2Short(value).hashCode()
    checkSideEffected()
  }

  @Test def hash2_SideEffect(): Unit = {
    {sideEffect(); value}.##
    checkSideEffected()
  }

  @Test def str_SideEffect1(): Unit = {
    {sideEffect(); value}.toString
    checkSideEffected()
  }

  @Test def str_SideEffect2(): Unit = {
    {sideEffect(); Predef}.short2Short(value).toString
    checkSideEffected()
  }

  @Test def float_SideEffect1(): Unit = {
    {sideEffect(); value}.floatValue()
    checkSideEffected()
  }
  @Test def float_SideEffect2(): Unit = {
    {sideEffect(); Predef}.short2Short(value).floatValue()
    checkSideEffected()
  }
  @Test def double_SideEffect1(): Unit = {
    {sideEffect(); value}.doubleValue()
    checkSideEffected()
  }
  @Test def double_SideEffect2(): Unit = {
    {sideEffect(); Predef}.short2Short(value).doubleValue()
    checkSideEffected()
  }
  @Test def long_SideEffect1(): Unit = {
    {sideEffect(); value}.longValue()
    checkSideEffected()
  }
  @Test def long_SideEffect2(): Unit = {
    {sideEffect(); Predef}.short2Short(value).longValue()
    checkSideEffected()
  }
  @Test def int_SideEffect1(): Unit = {
    {sideEffect(); value}.intValue()
    checkSideEffected()
  }
  @Test def int_SideEffect2(): Unit = {
    {sideEffect(); Predef}.short2Short(value).intValue()
    checkSideEffected()
  }
  @Test def short_SideEffect1(): Unit = {
    {sideEffect(); value}.shortValue()
    checkSideEffected()
  }
  @Test def short_SideEffect2(): Unit = {
    {sideEffect(); Predef}.short2Short(value).shortValue()
    checkSideEffected()
  }
  @Test def byte_SideEffect1(): Unit = {
    {sideEffect(); value}.byteValue()
    checkSideEffected()
  }
  @Test def byte_SideEffect2(): Unit = {
    {sideEffect(); Predef}.short2Short(value).byteValue()
    checkSideEffected()
  }
}
