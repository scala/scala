/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
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

import scala.annotation.{ nowarn, tailrec }
import scala.reflect.{ClassTag, classTag}

object AllocationTest {
  val threadMXBean = ManagementFactory.getThreadMXBean.asInstanceOf[com.sun.management.ThreadMXBean]
  assertTrue(threadMXBean.isThreadAllocatedMemorySupported)
  threadMXBean.setThreadAllocatedMemoryEnabled(true)

  @nowarn("cat=lint-nullary-unit")
  private object coster extends AllocationTest {
    def byte    = 99.toByte
    def short   = 9999.toShort
    def int     = 100000000
    def long    = 100000000000000L
    def boolean = true
    def char    = 's'
    def float   = 123456F
    def double  = 123456D
    def unit    = ()

    def sizeOf[T <: AnyRef](fn: => T): T = fn
  }

  lazy val costObject  = costOf(coster, "Object")
  lazy val costByte    = costOf(coster.byte, "Byte")
  lazy val costShort   = costOf(coster.short, "Short")
  lazy val costInt     = costOf(coster.int, "Int")
  lazy val costLong    = costOf(coster.long, "Long")
  lazy val costBoolean = costOf(coster.boolean, "Boolean")
  lazy val costChar    = costOf(coster.char, "Char")
  lazy val costFloat   = costOf(coster.float, "Float")
  lazy val costDouble  = costOf(coster.double, "Double")
  lazy val costUnit    = costOf(coster.unit, "Unit")

  def sizeOf[T <: AnyRef](fn: => T, msg: String, ignoreEqualCheck: Boolean = false): Long = {
    val size = coster.calcAllocationInfo(coster.sizeOf(fn), costObject, msg, ignoreEqualCheck).min
    println(s"size of $msg = $size")
    size
  }

  private def costOf[T](fn: => T, tpe: String): Long = {
    val cost = coster.calcAllocationInfo(fn, 0, "", ignoreEqualCheck = false).min
    println(s"cost of tracking allocations - cost of $tpe = $cost")
    cost
  }
}

trait AllocationTest {
  import AllocationTest._

  /** Asserts whether it's expected for `a == b` to allocate memory. */
  def nonAllocatingEqual(expected: Boolean, a: AnyRef, b: AnyRef): Unit = {
    assertEquals(expected, nonAllocating(Boolean.box(a == b)))
  }

  /** Asserts that the execution of `fn` does not allocate any memory. */
  def nonAllocating[T: ClassTag](fn: => T, text: String = "", ignoreEqualCheck: Boolean = false)(implicit execution: AllocationExecution = AllocationExecution()): T = {
    onlyAllocates(0, text, ignoreEqualCheck)(fn)
  }

  private def showAllocations(allocations: List[Long]): String = allocations match {
    case a :: allocations =>
      val sb = new StringBuilder
      def append(a: Long, count: Int) = sb.append(s" allocation $a ($count times)\n")
      def loop(allocations: List[Long], last: Long, count: Int): String = allocations match {
        case Nil => append(last, count).result()
        case b :: allocations =>
          val n = if (b != last) { append(b, count); 1 } else count + 1
          loop(allocations, b, n)
      }
      loop(allocations, a, 1)
    case _         => ""
  }

  /** Asserts that the execution of `fn` allocates `size` bytes or less. */
  def onlyAllocates[T: ClassTag](size: Long, text: String = "", ignoreEqualCheck: Boolean = false)(fn: => T)(implicit execution: AllocationExecution = AllocationExecution()): T = {
    val result = allocationInfo(fn, text, ignoreEqualCheck)
    if (result.min > size) failTest(size, text, result)
    result.result
  }

  /** Asserts that the execution of `fn` allocates exactly `size` bytes. */
  def exactAllocates[T: ClassTag](size: Long, text: String = "", ignoreEqualCheck: Boolean = false)(fn: => T)(implicit execution: AllocationExecution = AllocationExecution()): T = {
    val result = allocationInfo(fn, text, ignoreEqualCheck)
    if (result.min != size) failTest(size, text, result)
    result.result
  }

  private def failTest[T](size: Long, text: String, result: AllocationInfo[T]) = {
    val extraText  = if (text.isEmpty) "" else s" -- $text"
    def show(x: T) = if (x == null) "null" else s"$x (${x.getClass})"
    fail(s"""allocating min = ${result.min} allowed = $size$extraText
            | result = ${show(result.result)}
            |${showAllocations(result.allocations.toList)}""".stripMargin)
  }

  def allocationInfo[T: ClassTag](fn: => T, text: String = "", ignoreEqualCheck: Boolean = false)(implicit execution: AllocationExecution = AllocationExecution()): AllocationInfo[T] = {
    val cost = classTag[T].runtimeClass match {
      case cls if cls == classOf[Byte]    => costByte
      case cls if cls == classOf[Short]   => costShort
      case cls if cls == classOf[Int]     => costInt
      case cls if cls == classOf[Long]    => costLong
      case cls if cls == classOf[Boolean] => costBoolean
      case cls if cls == classOf[Char]    => costChar
      case cls if cls == classOf[Float]   => costFloat
      case cls if cls == classOf[Double]  => costDouble
      case cls if cls == classOf[Unit]    => costUnit
      case cls if cls.isPrimitive         => sys.error(s"Unexpected primitive $cls")
      case _                              => costObject
    }
    calcAllocationInfo(fn, cost, text, ignoreEqualCheck)
  }

  /** Calculates memory allocation exempting `cost` expected bytes (e.g. java.lang.Object overhead) */
  private[AllocationTest] def calcAllocationInfo[T](fn: => T, cost: Long, text: String, ignoreEqualCheck: Boolean)(implicit execution: AllocationExecution = AllocationExecution()): AllocationInfo[T] = {
    val expected  = fn
    val extraText = if (text.isEmpty) "" else s" -- $text"
    @annotation.nowarn("cat=deprecation")
    val id        = Thread.currentThread().getId
    val counts    = new Array[Long](execution.executionCount)

    @tailrec def warmupLoop(i: Int): Unit = if (i < execution.warmupCount) {
      val actual = fn
      if (!ignoreEqualCheck && actual != expected)
        assertEquals(s"warmup at index $i $expected $actual$extraText", expected, actual)
      warmupLoop(i + 1)
    }

    @tailrec def testLoop(i: Int): Unit = if (i < execution.executionCount) {
      val before = threadMXBean.getThreadAllocatedBytes(id)
      val actual = fn
      val after  = threadMXBean.getThreadAllocatedBytes(id)
      counts(i)  = after - cost - before
      if (!ignoreEqualCheck && actual != expected)
        assertEquals(s"at index $i $expected $actual$extraText", expected, actual)
      testLoop(i + 1)
    }

    warmupLoop(0)
    testLoop(0)
    AllocationInfo(expected, counts)
  }
}

case class AllocationExecution(executionCount: Int = 1000, warmupCount: Int = 1000)

case class AllocationInfo[T](result: T, allocations: Array[Long]) {
  def min: Long = allocations.min
}
