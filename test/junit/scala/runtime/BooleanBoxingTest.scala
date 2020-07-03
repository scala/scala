package scala.runtime

import org.junit.Test
import org.junit.Assert._

import scala.tools.testing.AllocationTest

class BooleanBoxingTest extends SideEffectTest with AllocationTest{
  val value = true

  @Test def hash1(): Unit = {
    nonAllocating(value.hashCode())
  }

  @Test def hash2(): Unit = {
    nonAllocating(value.##)
  }

  @Test def str(): Unit = {
    val cost = allocationInfo(java.lang.Boolean.toString(value), "", false)
    assertEquals("true", exactAllocates(cost.min)(value.toString()))
  }

  //check that any rewrites don't skip side effects
  @Test def hash1_SideEffect1(): Unit = {
    {sideEffect; value}.hashCode()
    checkSideEffected
  }
  @Test def hash1_SideEffect2(): Unit = {
    {sideEffect; Predef}.boolean2Boolean(value).hashCode()
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
    {sideEffect; Predef}.boolean2Boolean(value).toString
    checkSideEffected
  }

}
