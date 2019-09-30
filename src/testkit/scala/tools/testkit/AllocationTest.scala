/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.testkit

import java.lang.management.ManagementFactory

import org.junit.Assert.{assertEquals, assertTrue, fail}

object AllocationTest {
  val allocationCounter = ManagementFactory.getThreadMXBean.asInstanceOf[com.sun.management.ThreadMXBean]
  assertTrue(allocationCounter.isThreadAllocatedMemorySupported)
  allocationCounter.setThreadAllocatedMemoryEnabled(true)
  val costObject = {
    val id = Thread.currentThread().getId
    for (i <- 1 to 1000) yield {
      val before = allocationCounter.getThreadAllocatedBytes(id)
      val after = allocationCounter.getThreadAllocatedBytes(id)
      (after - before)
    }
  }.min
  val costInt: Long = {
    object coster extends AllocationTest
    val costs = coster.allocationInfoImpl(coster.hashCode(), new AllocationExecution(), 0)
    costs.min
  }


  println(s"cost of tracking allocations = $costObject")
}

trait AllocationTest {

  import AllocationTest._

  def nonAllocatingEqual(expected: Boolean, a: AnyRef, b: AnyRef): Unit = {
    assertEquals(expected, nonAllocating(java.lang.Boolean.valueOf(a == b)))
  }

  def nonAllocating[T: Manifest](fn: => T)(implicit execution: AllocationExecution = AllocationExecution()): T = {
    onlyAllocates(0)(fn)
  }

  def onlyAllocates[T: Manifest](size:Int)(fn: => T)(implicit execution: AllocationExecution = AllocationExecution()): T = {
    val result = allocationInfo(fn)

    if (result.min > size) {
//      result.allocations foreach {
//        x => println(s"allocation $x")
//      }
      fail(s"allocating min = ${result.min}")
    }
    result.result
  }

  def allocationInfo[T: Manifest](fn: => T)(implicit execution: AllocationExecution = AllocationExecution()): AllocationInfo[T] = {
    val cls = manifest[T].runtimeClass
    val cost =
      if (cls == classOf[Int]) costInt
      else if (cls.isPrimitive) ???
      else costObject
    allocationInfoImpl(fn, execution, cost)
  }

  private[AllocationTest] def allocationInfoImpl[T](fn: => T, execution: AllocationExecution, cost: Long): AllocationInfo[T] = {
    val expected = fn
    val id = Thread.currentThread().getId

    //warmup
    for (i <- 0 until execution.warmupCount) {
      val actual = fn
      assertEquals(s"warmup at index $i $expected $actual", expected, actual)
    }

   //test
    val counts = new Array[Long](execution.executionCount)
    for (i <- 0 until execution.executionCount) {
      val before = allocationCounter.getThreadAllocatedBytes(id)
      val actual = fn
      val after = allocationCounter.getThreadAllocatedBytes(id)
      counts(i) = after - cost - before
      assertEquals(s"at index $i $expected $actual", expected, actual)
    }
    AllocationInfo(expected, counts)
  }

}

case class AllocationExecution(executionCount: Int = 1000, warmupCount: Int = 1000)

case class AllocationInfo[T](result: T, allocations: Array[Long]) {
  def min = allocations.iterator.min
}
