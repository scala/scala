package scala.tools.testing


import java.lang.management.ManagementFactory

import org.junit.Assert.{assertEquals, assertTrue, fail}

object AllocationTest {
  val allocationCounter = ManagementFactory.getThreadMXBean.asInstanceOf[com.sun.management.ThreadMXBean]
  assertTrue(allocationCounter.isThreadAllocatedMemorySupported)
  allocationCounter.setThreadAllocatedMemoryEnabled(true)

  private object coster extends AllocationTest {
    def byte = 1.toByte

    def short = 1.toShort

    def int = 100000000

    def long = 100000000000000L

    def boolean = true

    def char = 's'

    def float = 1F

    def double = 1D

    def unit = ()
  }
  private def trace(Type:String, value:Long) :Long = {
    println(s"cost of tracking allocations - cost of $Type   = $value")
    value
  }

  lazy val costObject:  Long = trace("Object", coster.allocationInfoImpl(coster, new AllocationExecution(), 0).min)
  lazy val costByte:    Long =  trace("Byte", coster.allocationInfoImpl(coster.byte, new AllocationExecution(), 0).min)
  lazy val costShort:   Long =  trace("Short", coster.allocationInfoImpl(coster.short, new AllocationExecution(), 0).min)
  lazy val costInt:     Long =  trace("Int", coster.allocationInfoImpl(coster.int, new AllocationExecution(), 0).min)
  lazy val costLong:    Long =  trace("Long", coster.allocationInfoImpl(coster.long, new AllocationExecution(), 0).min)
  lazy val costBoolean: Long =  trace("Boolean", coster.allocationInfoImpl(coster.boolean, new AllocationExecution(), 0).min)
  lazy val costChar:    Long =  trace("Char", coster.allocationInfoImpl(coster.char, new AllocationExecution(), 0).min)
  lazy val costFloat:   Long =  trace("Float", coster.allocationInfoImpl(coster.float, new AllocationExecution(), 0).min)
  lazy val costDouble:  Long =  trace("Double", coster.allocationInfoImpl(coster.double, new AllocationExecution(), 0).min)
  lazy val costUnit:    Long =  trace("Unit", coster.allocationInfoImpl(coster.unit, new AllocationExecution(), 0).min)

}

trait AllocationTest {

  import AllocationTest._

  def nonAllocating[T: Manifest](fn: => T)(implicit execution: AllocationExecution = AllocationExecution()): T = {
    onlyAllocates(0)(fn)
  }

  def onlyAllocates[T: Manifest](size: Int)(fn: => T)(implicit execution: AllocationExecution = AllocationExecution()): T = {
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
      if (cls == classOf[Byte]) costByte
      else if (cls == classOf[Short]) costShort
      else if (cls == classOf[Int]) costInt
      else if (cls == classOf[Long]) costLong
      else if (cls == classOf[Boolean]) costBoolean
      else if (cls == classOf[Char]) costChar
      else if (cls == classOf[Float]) costFloat
      else if (cls == classOf[Double]) costDouble
      else if (cls == classOf[Unit]) costUnit
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
