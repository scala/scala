package scala.util

import java.lang.management.ManagementFactory

import org.junit.Assert.{assertEquals, assertTrue, fail}

object AllocationTest {
  val allocationCounter = ManagementFactory.getThreadMXBean.asInstanceOf[com.sun.management.ThreadMXBean]
  assertTrue(allocationCounter.isThreadAllocatedMemorySupported)
  allocationCounter.setThreadAllocatedMemoryEnabled(true)
  val cost = {
    val id = Thread.currentThread().getId
    for (i <- 1 to 1000) yield {
      val before = allocationCounter.getThreadAllocatedBytes(id)
      val after = allocationCounter.getThreadAllocatedBytes(id)
      (after - before)
    }
  }.min

  println(s"cost of tracking allocations = $cost")
}

trait AllocationTest {

  import AllocationTest._

  def nonAllocatingEqual(expected: Boolean, a: AnyRef, b: AnyRef): Unit = {
    assertEquals(expected, nonAllocating(a == b))
  }

  def nonAllocating[T](fn: => T)(implicit execution: AllocationExecution = AllocationExecution()): T = {
    val result = allocationInfo(fn)
    val expected = fn

    if (result.min != 0) {
      result.allocations foreach {
        x => println(s"allocation $x")
      }
      fail(s"allocating min = ${result.min}")
    }
    result.result
  }

  def allocationInfo[T](fn: => T)(implicit execution: AllocationExecution = AllocationExecution()): AllocationInfo[T] = {
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