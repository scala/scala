/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.testing

import scala.collection.mutable.ArrayBuffer

/**
 * <p>
 *   Unit testing methods in the spirit of
 *   <a href="http://www.junit.org/" target="_top">JUnit</a> framework.
 * </p>
 * <p>
 *   Use these classes like this:
 * </p>
 * <pre>
 * <b>import</b> scala.testing.SUnit
 * <b>import</b> SUnit._
 *
 * <b>class</b> MyTest(n: String) <b>extends</b> TestCase(n) {
 *
 *   <b>override def</b> runTest() = n <b>match</b> {
 *     <b>case</b> "myTest1" => assertTrue(<b>true</b>)
 *     <b>case</b> "myTest2" => assertTrue("hello", <b>false</b>)
 *   }
 * }
 *
 * <b>val</b> r = <b>new</b> TestResult()
 * suite.run(r)
 * <b>for</b> (tf &lt;- r.failures()) {
 *   println(tf.toString())
 * }
 * </pre>
 * <p>
 *   The trait <code>TestConsoleMain</code> contains this code as
 *   a <code>main</code> method, for convenience.
 * </p>
 *
 * @deprecated SUnit will be removed in 2.8.0. There are several free and
 * sophisticated testing frameworks for Scala available, examples are
 * "ScalaTest", "ScalaCheck" or "Specs".
 *
 * @author Burak Emir
 */
@deprecated
object SUnit {

  /** <p>
   *    Convenience trait, mix it in a <code>TestMain</code> object and
   *    implement "suite" to get this code.
   *  </p><pre>
   *  <b>val</b> r = <b>new</b> TestResult()
   *  suite.run(r)
   *  <b>for</b> (<b>val</b> tf &lt;- r.failures()) {
   *    println(tf.toString())
   *  </pre>
   */
  trait TestConsoleMain {
    def suite: TestSuite
    def main(args: Array[String]) {
      val r = new TestResult()
      suite.run(r)
      for (tf <- r.failures())
        println(tf.toString())
    }
  }

  /** a Test can be run with its result being collected */
  trait Test {
    def run(r: TestResult): Unit
  }

  /** The class <code>TestCase</code> defines the fixture to run multiple
   *  tests.
   *
   *  @param name ...
   */
  abstract class TestCase(val name: String) extends Test with Assert {

    protected def runTest(): Unit

    def run(r: TestResult) {
      try {
        runTest()
      } catch {
        case t:Throwable => r.addFailure(this, t)
      }
    }

    def setUp() {}

    def tearDown() {}

    override def toString() = name
  }

  /** The class <code>TestFailure</code> collects a failed test together
   *  with the thrown exception.
   */
  class TestFailure(val failedTest: Test, val thrownException: Throwable) {

    def this(p: (Test, Throwable)) = this(p._1, p._2)

    override def toString() =
      failedTest.toString() + " failed due to " + thrownException.toString()

    def trace(): String = thrownException.getStackTraceString

  }

  /** a TestResult collects the result of executing a test case */
  class TestResult {
    val buf = new ArrayBuffer[(Test, Throwable)]()

    def addFailure(test: Test, t: Throwable) {
      buf += ((test, t))
    }

    def failureCount() =
      buf.length

    def failures() =
      buf.iterator map { x => new TestFailure(x) }
  }

  /** The class <code>TestSuite</code> runs a composite of test cases.
   */
  class TestSuite(tests: Test*) extends Test {

    def this(names: Seq[String], constr: String => Test) =
      this((names map constr):_*)

    val buf = new ArrayBuffer[Test]()

    buf ++= tests

    def addTest(t: Test) {
      buf += t
    }

    def run(r: TestResult) {
      for (t <- buf) t.run(r)
    }
  }

  /** an AssertFailed is thrown for a failed assertion */
  case class AssertFailed(msg: String, stackTrace: Boolean) extends RuntimeException {
    private val msg0 = if (stackTrace) {
      import java.io._
      val wrt = new StringWriter
      printStackTrace(new PrintWriter(wrt))
      wrt.toString
    } else msg
    override def toString() =
      if (msg0 eq null) "failed assertion: " + msg else msg0
  }

  /** this class defines useful <code>assert</code> methods */
  trait Assert {

    def enableStackTrace: Boolean = true

    /** fails if <code>! actual.sameElements(expected)</code> */
    def assertSameElements[A](actual: Seq[A], expected: Seq[A]) {
      if (! actual.sameElements(expected))
        fail("(no message)", actual.toString, expected.toString)
    }

    /** fails if expected != actual */
    def assertEquals[A](msg: String, expected: A, actual: A) {
      if (expected != actual) fail(msg, expected, actual)
    }

    /** fails if expected != actual */
    def assertEquals[A](expected: A, actual: A) {
      assertEquals("(no message)", expected, actual)
    }

    /** succeeds if actual is false */
    def assertFalse(msg: String, actual: Boolean) {
      assertEquals(msg, false, actual)
    }

    /** succeeds if actual is false */
    def assertFalse(actual: Boolean) {
      assertFalse("(no message)", actual)
    }

    /** fails if null eq actual */
    def assertNotNull(msg: String, actual: AnyRef) {
      if (null eq actual) fail(msg)
    }

    /** fails if null eq actual */
    def assertNotNull(actual: AnyRef): Unit  =
      assertNotNull("(no message)", actual)

    /** fails if <code>expected eq actual</code> */
    def assertNotEq(msg: String, expected: AnyRef, actual: AnyRef) {
      if (expected eq actual) fail(msg)
    }

    /** fails if <code>expected eq actual</code> */
    def assertNotEq(expected: AnyRef, actual: AnyRef) {
      assertNotEq("(no message)", expected, actual)
    }

    /** fails if actual ne null */
    def assertNull(msg: String, actual: AnyRef) {
      if (null ne actual) fail(msg)
    }

    /** fails if actual ne null */
    def assertNull(actual: AnyRef) {
      assertNull("(no message)", actual)
    }

    /** fails if <code>expected ne actual</code> */
    def assertEq(msg: String, expected: AnyRef, actual: AnyRef) {
      if (expected ne actual) fail(msg)
    }

    /** fails if expected ne actual */
    def assertEq(expected: AnyRef, actual: AnyRef) {
      assertEq("(no message)", expected, actual)
    }

    /** succeeds if actual == true */
    def assertTrue(msg: String, actual: Boolean) {
      assertEquals(msg, true, actual)
    }

    /** succeeds if actual == true */
    def assertTrue(actual: Boolean) {
      assertTrue("(no message)", actual)
    }

    /** throws <code>AssertFailed</code> with given message <code>msg</code>.
     */
    def fail(msg: String) {
      throw AssertFailed(msg, enableStackTrace)
    }

    def fail[A](msg: String, expected: A, actual: A) {
      throw AssertFailed(msg +
                         ", expected: " + expected +
                         ", actual: " + actual, enableStackTrace)
    }
  }
}
