package scala.runtime

import org.junit.Assert._
import org.junit.Test

import scala.annotation.nowarn
import scala.tools.testkit.AllocationTest

class CharBoxingTest extends SideEffectTest with AllocationTest {
  val value = 'x'

  @nowarn("cat=w-flag-value-discard")
  @Test def hash1(): Unit = nonAllocating(value.hashCode())

  @nowarn("cat=w-flag-value-discard")
  @Test def hash2(): Unit = nonAllocating(value.##)

  @Test def str(): Unit = {
    val cost = allocationInfo(java.lang.Character.toString(value))
    assertEquals("x", exactAllocates(cost.min)(value.toString()))
  }

  //check that any rewrites don't skip side effects
  @Test def hash1_SideEffect1(): Unit = {
    {sideEffect(); value}.hashCode()
    checkSideEffected()
  }

  @Test def hash1_SideEffect2(): Unit = {
    {sideEffect(); Predef}.char2Character(value).hashCode()
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
    {sideEffect(); Predef}.char2Character(value).toString
    checkSideEffected()
  }
}
