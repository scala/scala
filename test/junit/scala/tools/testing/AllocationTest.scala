package scala.tools.testing


import java.lang.management.ManagementFactory

import org.junit.Assert.{assertEquals, assertTrue, fail}

import scala.reflect.{ClassTag, classTag}

object AllocationTest {
  val allocationCounter = ManagementFactory.getThreadMXBean.asInstanceOf[com.sun.management.ThreadMXBean]
  assertTrue(allocationCounter.isThreadAllocatedMemorySupported)
  allocationCounter.setThreadAllocatedMemoryEnabled(true)

  private object coster extends AllocationTest {
    def byte = 99.toByte

    def short = 9999.toShort

    def int = 100000000

    def long = 100000000000000L

    def boolean = true

    def char = 's'

    def float = 123456F

    def double = 123456D

    def unit = ()

    def sizeOf[T <: AnyRef](fn: => T): T = fn
  }
  private def trace(Type:String, value:Long) :Long = {
    println(s"cost of tracking allocations - cost of $Type   = $value")
    value
  }

  lazy val costObject:  Long = trace("Object", coster.allocationInfoImpl(coster, new AllocationExecution(), 0, "", false).min)
  lazy val costByte:    Long =  trace("Byte", coster.allocationInfoImpl(coster.byte, new AllocationExecution(), 0, "", false).min)
  lazy val costShort:   Long =  trace("Short", coster.allocationInfoImpl(coster.short, new AllocationExecution(), 0, "", false).min)
  lazy val costInt:     Long =  trace("Int", coster.allocationInfoImpl(coster.int, new AllocationExecution(), 0, "", false).min)
  lazy val costLong:    Long =  trace("Long", coster.allocationInfoImpl(coster.long, new AllocationExecution(), 0, "", false).min)
  lazy val costBoolean: Long =  trace("Boolean", coster.allocationInfoImpl(coster.boolean, new AllocationExecution(), 0, "", false).min)
  lazy val costChar:    Long =  trace("Char", coster.allocationInfoImpl(coster.char, new AllocationExecution(), 0, "", false).min)
  lazy val costFloat:   Long =  trace("Float", coster.allocationInfoImpl(coster.float, new AllocationExecution(), 0, "", false).min)
  lazy val costDouble:  Long =  trace("Double", coster.allocationInfoImpl(coster.double, new AllocationExecution(), 0, "", false).min)
  lazy val costUnit:    Long =  trace("Unit", coster.allocationInfoImpl(coster.unit, new AllocationExecution(), 0, "", false).min)

  def sizeOf[T <: AnyRef](fn: => T, msg: String, ignoreEqualCheck: Boolean = false): Long = {
    val size = coster.allocationInfoImpl(coster.sizeOf(fn), new AllocationExecution(), costObject, msg, ignoreEqualCheck).min
    println(s"size of $msg = $size")
    size
  }

}

trait AllocationTest {

  import AllocationTest._

  def nonAllocating[T: ClassTag](fn: => T, text: String = "", ignoreEqualCheck: Boolean = false)(implicit execution: AllocationExecution = AllocationExecution()): T = {
    onlyAllocates(0, text, ignoreEqualCheck)(fn)
  }
  private def printAllocations[T: ClassTag](result: AllocationInfo[T]): String = {
    var last = -1L
    var count = 0
    val sb = new StringBuilder
    sb.append("Start allocation detail\n")
    def printNow(next: Long): Unit = {
      if (last != -1)
        sb.append(s"allocation $last ($count times)\n")
      last = next
      count = 1
    }
    result.allocations foreach {
      a =>
        if (a == last) count += 1
        else printNow(a)
    }
    printNow(-1)
    sb.append("End allocation detail")
    sb.toString
  }

  def onlyAllocates[T: ClassTag](size: Long, text: String = "", ignoreEqualCheck: Boolean = false)(fn: => T)(implicit execution: AllocationExecution = AllocationExecution()): T = {
    val result = allocationInfo(fn, text, ignoreEqualCheck)

    if (result.min > size) {
      fail(s"allocating min = ${result.min} allowed = ${size}${if (text.isEmpty) "" else s" -- $text"}\n result was ${result.result}\n result class ${if (result.result == null) "<null>" else result.result.getClass.getName}\n${printAllocations(result)}")
    }
    result.result
  }
  def exactAllocates[T: ClassTag](size:Long, text: String = "", ignoreEqualCheck: Boolean = false)(fn: => T)(implicit execution: AllocationExecution = AllocationExecution()): T = {
    val result = allocationInfo(fn, text, ignoreEqualCheck)

    if (result.min != size) {
      fail(s"allocating min = ${result.min} allowed = ${size}${if (text.isEmpty) "" else s" -- $text"}\n result was ${result.result}\n result class ${if (result.result == null) "<null>" else result.result.getClass.getName}\n${printAllocations(result)}")
    }
    result.result
  }

  def allocationInfo[T: ClassTag](fn: => T, text: String, ignoreEqualCheck: Boolean)(implicit execution: AllocationExecution = AllocationExecution()): AllocationInfo[T] = {
    val cls = classTag[T].runtimeClass
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
    allocationInfoImpl(fn, execution, cost, text, ignoreEqualCheck)
  }

  private[AllocationTest] def allocationInfoImpl[T](fn: => T, execution: AllocationExecution, cost: Long, text: String, ignoreEqualCheck: Boolean): AllocationInfo[T] = {
    val expected = fn
    val id = Thread.currentThread().getId

    var i = 0
    //warmup
    while (i < execution.warmupCount) {
      val actual = fn
      if (!ignoreEqualCheck && actual != expected)
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
      if (!ignoreEqualCheck && actual != expected)
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
