package scala.util

import scala.tools.nsc.profile.ExtendedThreadMxBean
import org.junit.Assert._

object Allocations {
  val threadMx = ExtendedThreadMxBean.proxy()
  if (threadMx.isThreadAllocatedMemorySupported) threadMx.setThreadAllocatedMemoryEnabled(true)

  //warmup
  for (i <- 1 to 100) allocations(i)
  //calibrate
  val bytes = (((1 to 100).map { i => allocations(i)}).unzip)._2
  assert(bytes.min == bytes.max)
  val expected = bytes.head
  private def allocations[T]( fn: => T) : (T,Long) = {
    require (threadMx.isThreadAllocatedMemoryEnabled, "allocation counting is not enabled")
    val currentThread = Thread.currentThread().getId
    val before = threadMx.getThreadAllocatedBytes(currentThread)
    val res = fn
    val after = threadMx.getThreadAllocatedBytes((currentThread))
    (res, after - before - expected)
  }

}
trait Allocations {
  import Allocations._
  def assertNonAllocating[T]( fn: => T) : T = {
    val (result, bytes) = allocations(fn)
    if (bytes != 0) fail (s"expected no allocations but the thread allocated $bytes bytes")
    result
  }
  def resultAndAllocations[T]( fn: => T) : (T, Long) = {
    allocations(fn)
  }
}
