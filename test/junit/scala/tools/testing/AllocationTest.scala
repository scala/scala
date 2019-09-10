package scala.tools.testing


import java.lang.management.ManagementFactory

import org.junit.Assert.{assertEquals, assertTrue, fail}

object AllocationTest {
  val allocationCounter = ManagementFactory.getThreadMXBean.asInstanceOf[com.sun.management.ThreadMXBean]
  assertTrue(allocationCounter.isThreadAllocatedMemorySupported)
  allocationCounter.setThreadAllocatedMemoryEnabled(true)
  val costObject: Long = {
    object coster extends AllocationTest
    val costs = coster.allocationInfoImpl("" equals "xx", new AllocationExecution(), 0)
    costs.min
  }
  val costInt: Long = {
    object coster extends AllocationTest
    val costs = coster.allocationInfoImpl(coster.hashCode(), new AllocationExecution(), 0)
    costs.min
  }

  println(s"cost of tracking allocations - Object = $costObject")
  println(s"cost of tracking allocations - Int = $costInt")
}

trait AllocationTest {

  import AllocationTest._

  def nonAllocating[T: Manifest](fn: => T)(implicit execution: AllocationExecution = AllocationExecution()): T = {
    onlyAllocates(0)(fn)
  }
  def onlyAllocates[T: Manifest](size:Int)(fn: => T)(implicit execution: AllocationExecution = AllocationExecution()): T = {
    val result = allocationInfo(fn)

    if (result.min > size) {
      result.allocations foreach {
        x => println(s"allocation $x")
      }
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

    var i = 0
    //warmup
    while (i < execution.warmupCount) {
      val actual = fn
      if (actual != expected)
        assertEquals(s"warmup at index $i $expected $actual", expected, actual)
      i += 1
    }

    //test
    i = 0
    val counts = new Array[Long](execution.executionCount)
    while (i < execution.warmupCount) {
      val before: Long = allocationCounter.getThreadAllocatedBytes(id)
      val actual = fn
      val after: Long = allocationCounter.getThreadAllocatedBytes(id)
      counts(i) = after - cost - before
      if (actual != expected)
        assertEquals(s"at index $i $expected $actual", expected, actual)
      i += 1
    }
    AllocationInfo(expected, counts)
  }

}

case class AllocationExecution(executionCount: Int = 1000, warmupCount: Int = 1000)

case class AllocationInfo[T](result: T, allocations: Array[Long]) {
  def min: Long = {
    var min = allocations(0)
    var i = 1
    while (i < allocations.length) {
      min = Math.min(min, allocations(i))
      i += i
    }
    min
  }
}
