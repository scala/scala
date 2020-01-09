
package scala.tools.nsc.util

import scala.util.{Failure, Success, Try}

import org.junit.Assert.{assertEquals, assertTrue}
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class StackTraceTest {
  val CausedBy   = "Caused by: "
  val Suppressed = "Suppressed: "

  // throws
  def sample = throw new RuntimeException("Point of failure")
  def sampler: String = sample

  // repackage with message
  def resample: String = try sample catch { case e: Throwable => throw new RuntimeException("resample", e) }
  def resampler: String = resample

  // simple wrapper
  def wrapper: String = try sample catch { case e: Throwable => throw new RuntimeException(e) }
  // another onion skin
  def rewrapper: String = try wrapper catch { case e: Throwable => throw new RuntimeException(e) }
  def rewrapperer: String = rewrapper

  // circular cause
  def insane: String = try sample catch {
    case e: Throwable =>
      val t = new RuntimeException(e)
      e initCause t
      throw t
  }
  def insaner: String = insane

  def repressed: String = try sample catch {
    case e: Throwable =>
      val t = new RuntimeException("My problem")
      t.addSuppressed(e)
      throw t
  }
  def represser: String = repressed

  // evaluating s should throw, p trims stack trace, t is the test of resulting trace string
  def probe(s: => String)(p: StackTraceElement => Boolean)(t: String => Unit): Unit = {
    Try(s).recover { case e => e.stackTracePrefixString(p) } match {
      case Success(s) => t(s)
      case Failure(e) => throw e
    }
  }

  @Test def showsAllTrace(): Unit =
    probe(sampler)(_ => true)(s => assertTrue(s.linesIterator.length > 5))

  // summary + one frame + elision
  @Test def showsOnlyPrefix() =
    probe(sample)(_.getMethodName == "sample")(s => assertEquals(3, s.linesIterator.length))

  // summary + one frame + elision, caused by + one frame + elision
  @Test def showsCause() = probe(resampler)(_.getMethodName != "resampler") { s =>
    val res = s.linesIterator.toList
    assertEquals(6, res.length)
    assertTrue(res.exists(_.startsWith(CausedBy)))
  }

  // summary + one frame + elision times three
  @Test def showsWrappedExceptions() = probe(rewrapperer)(_.getMethodName != "rewrapperer") { s =>
    val res = s.linesIterator.toList
    assertEquals(9, res.length)
    assertTrue(res.exists(_.startsWith(CausedBy)))
    assertEquals(2, res.collect { case s if s.startsWith(CausedBy) => s }.size)
  }

  // summary + one frame + elision times two with extra frame
  @Test def dontBlowOnCycle() = probe(insaner)(_.getMethodName != "insaner") { s =>
    val res = s.linesIterator.toList
    assertEquals(7, res.length)
    assertTrue(res.exists(_.startsWith(CausedBy)))
  }

  /** Java 7, but shouldn't bomb on Java 6.
   *
java.lang.RuntimeException: My problem
  at scala.tools.nsc.util.StackTraceTest.repressed(StackTraceTest.scala:56)
  ... 27 elided
  Suppressed: java.lang.RuntimeException: Point of failure
    at scala.tools.nsc.util.StackTraceTest.sample(StackTraceTest.scala:29)
    at scala.tools.nsc.util.StackTraceTest.repressed(StackTraceTest.scala:54)
    ... 27 more
  */
  @Test def showsSuppressed() = probe(represser)(_.getMethodName != "represser") { s =>
    val res = s.linesIterator.toList
    assertEquals(7, res.length)
    assertTrue(res.exists(_.trim.startsWith(Suppressed)))
  }
}
