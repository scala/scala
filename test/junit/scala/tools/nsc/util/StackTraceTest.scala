
package scala.tools.nsc.util

import scala.language.reflectiveCalls
import scala.util._
import PartialFunction.cond
import Properties.isJavaAtLeast

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

trait Expecting {
  /*
  import org.expecty.Expecty
  final val expect = new Expecty
  */
}


@RunWith(classOf[JUnit4])
class StackTraceTest extends Expecting {
  // formerly an enum
  val CausedBy   = "Caused by: "
  val Suppressed = "Suppressed: "

  // throws
  def sample = throw new RuntimeException("Point of failure")
  def sampler: String = sample

  // repackage with message
  def resample: String = try { sample } catch { case e: Throwable => throw new RuntimeException("resample", e) }
  def resampler: String = resample

  // simple wrapper
  def wrapper: String = try { sample } catch { case e: Throwable => throw new RuntimeException(e) }
  // another onion skin
  def rewrapper: String = try { wrapper } catch { case e: Throwable => throw new RuntimeException(e) }
  def rewrapperer: String = rewrapper

  // only an insane wretch would do this
  def insane: String = try { sample } catch {
    case e: Throwable =>
      val t = new RuntimeException(e)
      e initCause t
      throw t
  }
  def insaner: String = insane

  /** Java 7 */
  val suppressable = isJavaAtLeast("1.7")
  type Suppressing = { def addSuppressed(t: Throwable): Unit }

  def repressed: String = try { sample } catch {
    case e: Throwable =>
      val t = new RuntimeException("My problem")
      if (suppressable) {
        t.asInstanceOf[Suppressing] addSuppressed e
      }
      throw t
  }
  def represser: String = repressed

  // evaluating s should throw, p trims stack trace, t is the test of resulting trace string
  def probe(s: =>String)(p: StackTraceElement => Boolean)(t: String => Unit): Unit = {
    Try(s) recover { case e => e stackTracePrefixString p } match {
      case Success(s) => t(s)
      case Failure(e) => throw e
    }
  }

  @Test def showsAllTrace() {
    probe(sampler)(_ => true) { s =>
      val res = s.lines.toList
      /*
      expect {
        res.length > 5  // many lines
        // these expectations may be framework-specific
        //s contains "sbt.TestFramework"
        //res.last contains "java.lang.Thread"
      }
      */
      assert (res.length > 5)
    }
  }
  @Test def showsOnlyPrefix() = probe(sample)(_.getMethodName == "sample") { s =>
    val res = s.lines.toList
    /*
    expect {
      res.length == 3   // summary + one frame + elision
    }
    */
    assert (res.length == 3)
  }
  @Test def showsCause() = probe(resampler)(_.getMethodName != "resampler") { s =>
    val res = s.lines.toList
    /*
    expect {
      res.length == 6   // summary + one frame + elision, caused by + one frame + elision
      res exists (_ startsWith CausedBy.toString)
    }
    */
    assert (res.length == 6)
    assert (res exists (_ startsWith CausedBy.toString))
  }
  @Test def showsWrappedExceptions() = probe(rewrapperer)(_.getMethodName != "rewrapperer") { s =>
    val res = s.lines.toList
    /*
    expect {
      res.length == 9   // summary + one frame + elision times three
      res exists (_ startsWith CausedBy.toString)
      (res collect {
        case s if s startsWith CausedBy.toString => s
      }).size == 2
    }
    */
    assert (res.length == 9)
    assert (res exists (_ startsWith CausedBy.toString))
    assert ((res collect {
        case s if s startsWith CausedBy.toString => s
      }).size == 2)
  }
  @Test def dontBlowOnCycle() = probe(insaner)(_.getMethodName != "insaner") { s =>
    val res = s.lines.toList
    /*
    expect {
      res.length == 7   // summary + one frame + elision times two with extra frame
      res exists (_ startsWith CausedBy.toString)
    }
    */
    assert (res.length == 7)
    assert (res exists (_ startsWith CausedBy.toString))
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
    val res = s.lines.toList
    if (suppressable) {
      assert (res.length == 7)
      assert (res exists (_.trim startsWith Suppressed.toString))
    }
    /*
    expect {
      res.length == 7
      res exists (_ startsWith "  " + Suppressed.toString)
    }
    */
  }
}
