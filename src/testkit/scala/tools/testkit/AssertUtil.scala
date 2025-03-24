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

import org.junit.Assert.{assertEquals, assertNotEquals}
import org.junit.Assert.{assertFalse, assertTrue}
import org.junit.Assume.{assumeFalse, assumeTrue}

import scala.annotation.nowarn
import scala.collection.mutable
import scala.concurrent.{Await, Awaitable}
import scala.reflect.ClassTag
import scala.runtime.BoxesRunTime
import scala.runtime.ScalaRunTime.stringOf
import scala.util.{Failure, Success, Try}
import scala.util.Properties.isWin
import scala.util.chaining._
import scala.util.control.{ControlThrowable, NonFatal}
import java.lang.ref.{Reference, ReferenceQueue, SoftReference}
import java.lang.reflect.{Array => _, _}
import java.time.Duration
import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicReference
import java.util.IdentityHashMap

/** This module contains additional higher-level assert statements
 *  that are ultimately based on junit.Assert primitives.
 *
 *  Avoid adding methods, and above all *fields* (including `lazy val`s), that
 *  require JVM-specific features such as run-time reflection. Otherwise, all
 *  tests using this object stop working in Scala.js. Put such methods in
 *  `ReflectUtil` instead. (`ClassTag`s are fine; they are supported in
 *  Scala.js and Scala Native.)
 */
object AssertUtil {

  // junit fail is Unit
  def fail(message: String): Nothing = throw new AssertionError(message)

  def noWin(message: String = "skipping test on Windows")(body: => Unit) = {
    assumeFalse(message, isWin)
    body
  }

  private val Bail = new ControlThrowable {}

  def bail(): Nothing = throw Bail

  // if the test bails out, communicate a violated assumption
  def bailable(name: String)(test: => Unit): Unit =
    try test
    catch { case _: Bail.type => assumeTrue(s"$name skipped bail!", false) }

  private val printable = raw"\p{Print}".r

  def hexdump(s: String): Iterator[String] = {
    import scala.io.Codec
    val codec: Codec = Codec.UTF8
    var offset = 0
    def hex(bytes: Array[Byte])   = bytes.map(b => f"$b%02x").mkString(" ")
    def charFor(byte: Byte): Char = byte.toChar match { case c @ printable() => c ; case _ => '.' }
    def ascii(bytes: Array[Byte]) = bytes.map(charFor).mkString
    def format(bytes: Array[Byte]): String =
      f"$offset%08x  ${hex(bytes.slice(0, 8))}%-24s ${hex(bytes.slice(8, 16))}%-24s |${ascii(bytes)}|"
        .tap(_ => offset += bytes.length)
    s.getBytes(codec.charSet).grouped(16).map(format)
  }

  private def dump(s: String) = hexdump(s).mkString("\n")
  def assertEqualStrings(expected: String)(actual: String) =
    assert(expected == actual, s"Expected:\n${dump(expected)}\nActual:\n${dump(actual)}")

  // assertEquals but use BoxesRunTime.equals
  // let junit format a message on failure
  def assertEqualsAny(expected: Any, actual: Any): Unit =
    if (!BoxesRunTime.equals(expected, actual)) assertEquals(expected, actual)
  // as a bonus, message is by-name, though retains junit parameter order
  def assertEqualsAny(message: => String, expected: Any, actual: Any): Unit =
    if (!BoxesRunTime.equals(expected, actual)) assertEquals(message, expected, actual)
  def assertNotEqualsAny(expected: Any, actual: Any): Unit =
    if (BoxesRunTime.equals(expected, actual)) assertNotEquals(expected, actual)
  def assertNotEqualsAny(message: => String, expected: Any, actual: Any): Unit =
    if (BoxesRunTime.equals(expected, actual)) assertNotEquals(message, expected, actual)

  private final val timeout = 60 * 1000L                 // wait a minute

  private implicit class `ref helper`[A <: AnyRef](val r: Reference[A]) extends AnyVal {
    def isEmpty: Boolean  = r.get == null // r.refersTo(null) to avoid influencing collection
    def nonEmpty: Boolean = !isEmpty
    def hasReferent(x: AnyRef): Boolean = r.get eq x
  }
  private implicit class `class helper`(val clazz: Class[_]) extends AnyVal {
    def allFields: List[Field] = {
      def loop(k: Class[_]): List[Field] =
        if (k == null) Nil
        else k.getDeclaredFields.toList ::: loop(k.getSuperclass)
      loop(clazz)
    }
  }
  private implicit class `field helper`(val f: Field) extends AnyVal {
    def follow(o: AnyRef): AnyRef = {
      f setAccessible true
      f get o
    }
  }

  /** Result and elapsed duration.
   */
  def timed[A](body: => A): (A, Duration) = {
    val start = System.nanoTime
    val result = body
    val end = System.nanoTime
    (result, Duration.ofNanos(end - start))
  }

  /** Elapsed duration.
   */
  def elapsed[U](body: => U): Duration = timed(body)._2

  /** Elapsed duration.
   */
  def withElapsed[A](f: Duration => Unit)(body: => A): A = timed(body).pipe {
    case (result, duration) => f(duration) ; result
  }

  /** Expect the exception is thrown by evaluating `body`.
   */
  def intercept[T <: Throwable: ClassTag](body: => Any): Unit = assertThrown[T](_ => true)(body)

  /** Expect the exception thrown with exactly the given message.
   */
  def interceptMessage[T <: Throwable: ClassTag](expected: String)(body: => Any): Unit =
    assertThrown[T](_.getMessage == expected)(body)

  /** Check that throwable T (or a subclass) was thrown during evaluation of `body`,
   *  and that its message satisfies the `checkMessage` predicate.
   *  Any other exception is propagated.
   */
  def assertThrows[T <: Throwable: ClassTag](body: => Any, checkMessage: String => Boolean = _ => true): Unit =
    assertThrown[T](t => checkMessage(t.getMessage))(body)

  private val Unthrown = new ControlThrowable {}

  /** Assert that the exception was thrown while evaluating `body`,
   *  and that the exception instance satisfies the `checker` predicate.
   */
  def assertThrown[T <: Throwable: ClassTag](checker: T => Boolean)(body: => Any): Unit =
    try {
      body
      throw Unthrown
    } catch {
      case Unthrown => fail("Expression did not throw!")
      case e: T if checker(e) => ()
      case failed: T =>
        val ae = new AssertionError(s"Exception failed check: $failed")
        ae.addSuppressed(failed)
        throw ae
      case NonFatal(other) =>
        val ae = new AssertionError(s"Wrong exception: expected ${implicitly[ClassTag[T]]} but was ${other.getClass.getName}")
        ae.addSuppressed(other)
        throw ae
    }

  def assertCond[A](x: A)(pf: PartialFunction[A, Boolean]): Unit    = assertTrue(PartialFunction.cond(x)(pf))
  def assertCondNot[A](x: A)(pf: PartialFunction[A, Boolean]): Unit = assertFalse(PartialFunction.cond(x)(pf))

  def assertFails[U](checkMessage: String => Boolean)(body: => U): Unit = assertThrows[AssertionError](body, checkMessage)

  private def orEmpty(b: Boolean)(text: => String): String = if (b) text else ""

  /** JUnit-style assertion for `Seq#sameElements`.
   *  The `actual` is iterated twice if failure is reported.
   */
  def assertSameElements[A, B >: A](expected: Seq[A], actual: Iterable[B], message: String = ""): Unit =
    if (!expected.sameElements(actual))
      fail(f"${orEmpty(message.nonEmpty)(s"$message ")}expected:<${stringOf(expected)}> but was:<${stringOf(actual)}>")

  /** Convenience for testing iterators and non-Seqs.
   *  The `actual` is collected to a `List` for reporting errors.
   */
  def assertSameElements[A, B >: A](expected: Seq[A], actual: IterableOnce[B]): Unit =
    assertSameElements(expected, actual.iterator.to(Iterable), message = "")

  /** Convenience for testing iterators and non-Seqs.
   *  The `expected` is collected to a `List` for reporting errors.
   */
  def assertSameElements[A, B >: A](expected: IterableOnce[A], actual: IterableOnce[B]): Unit =
    assertSameElements(expected.iterator.to(Seq), actual)

  /** Convenience for testing arrays. Avoids warning about implicit conversion to Seq.
   */
  def assertSameElements[A, B >: A](expected: Array[A], actual: Array[B]): Unit =
    assertSameElements(expected, actual, message = "")

  def assertSameElements[A, B >: A](expected: Array[A], actual: Array[B], message: String): Unit =
    assertSameElements(expected.toIndexedSeq, actual.toIndexedSeq, message)

  /** Value is not strongly reachable from roots after body is evaluated.
   */
  def assertNotReachable[A <: AnyRef](a: => A, roots: AnyRef*)(body: => Unit): Unit = {
    val refq = new ReferenceQueue[A]
    val ref: Reference[A] = new SoftReference(a, refq)
    // fail if following strong references from root discovers referent. Quit if ref is empty.
    def assertNoRef(root: AnyRef): Unit = {
      val seen  = new IdentityHashMap[AnyRef, Unit]
      val stack = mutable.Stack.empty[AnyRef]
      def loop(): Unit = if (ref.nonEmpty && stack.nonEmpty) {
        val o: AnyRef = stack.pop()
        if (o != null && !seen.containsKey(o)) {
          seen.put(o, ())
          assertFalse(s"Root $root held reference $o", ref.hasReferent(o))
          o match {
            case a: Array[AnyRef] =>
              a.foreach(e => if (e != null && !e.isInstanceOf[Reference[_]]) stack.push(e))
            case _ =>
              for (f <- o.getClass.allFields)
                if (!Modifier.isStatic(f.getModifiers) && !f.getType.isPrimitive && !classOf[Reference[_]].isAssignableFrom(f.getType))
                  stack.push(f.follow(o))
          }
        }
        refq.poll() match {
          case null  =>
          case r @ _ => fail("assertNotReachable dropped reference value")
        }
        loop()
      }
      stack.push(root)
      loop()
    }
    body
    roots.foreach(assertNoRef)
  }

  /** Assert no new threads, with some margin for arbitrary threads to exit. */
  def assertZeroNetThreads(body: => Unit): Unit = {
    val group = new ThreadGroup("junit")
    try assertZeroNetThreads(group)(body)
    finally group.destroy(): @nowarn("cat=deprecation") // deprecated since JDK 16, will be removed
  }
  def assertZeroNetThreads[A](group: ThreadGroup)(body: => A): Try[A] = {
    val testDone = new CountDownLatch(1)
    def check(): Try[A] = {
      val beforeCount = group.activeCount
      val beforeThreads = new Array[Thread](beforeCount)
      assertEquals("Spurious early thread creation.", beforeCount, group.enumerate(beforeThreads))

      val outcome = Try(body)

      val afterCount = {
        waitForIt(group.activeCount <= beforeCount, label = "after count")
        group.activeCount
      }
      val afterThreads = new Array[Thread](afterCount)
      assertEquals("Spurious late thread creation.", afterCount, group.enumerate(afterThreads))
      val staleThreads = afterThreads.toList.diff(beforeThreads)
      //staleThreads.headOption.foreach(_.getStackTrace.foreach(println))
      val staleMessage = staleThreads.mkString("There are stale threads: ",",","")
      assertEquals(staleMessage, beforeCount, afterCount)
      assertTrue(staleMessage, staleThreads.isEmpty)

      outcome
    }
    val result = new AtomicReference[Try[A]]()
    def test(): Try[A] =
      try {
        val checked = check()
        result.set(checked)
        checked
      } finally {
        testDone.countDown()
      }

    val timeout = 10 * 1000L
    val thread = new Thread(group, () => test())
    def abort(): Try[A] = {
      group.interrupt()
      new Failure(new AssertionError("Test did not complete"))
    }
    try {
      thread.start()
      waitForIt(testDone.getCount == 0, Fast, label = "test result")
      if (testDone.await(timeout, TimeUnit.MILLISECONDS))
        result.get
      else
        abort()
    } finally {
      thread.join(timeout)
    }
  }

  /** Wait for a condition, with a simple back-off strategy.
   *
   *  This makes it easier to see hanging threads in development
   *  without tweaking a timeout parameter. Conversely, when a thread
   *  fails to make progress in a test environment, we allow the wait
   *  period to grow larger than usual, since a long wait for failure
   *  is acceptable.
   *
   *  It would be nicer if what we're waiting for gave us
   *  a progress indicator: we don't care if something
   *  takes a long time, so long as we can verify progress.
   */
  def waitForIt(terminated: => Boolean, progress: Progress = Fast, label: => String = "test"): Unit = {
    def value: Option[Boolean] = if (terminated) Some(true) else None
    assertTrue(waitFor(value, progress, label))
  }
  /** Wait for a value or eventually throw.
   */
  def waitFor[A](value: => Option[A], progress: Progress = Fast, label: => String = "test"): A = {
    val limit = 5
    var n = 1
    var (dormancy, factor) = progress match {
      case Slow => (10000L, 5)
      case Fast => (250L, 4)
    }
    var period = 0L
    var result: Option[A] = None
    var done = false
    while (!done && n < limit) {
      try {
        result = value
        done = result.nonEmpty
        if (!done) {
          //println(s"Wait for test condition: $label")
          Thread.sleep(dormancy)
          period += dormancy
        }
      } catch {
        case _: InterruptedException => done = true
      }
      n += 1
      dormancy *= factor
    }
    result match {
      case Some(v) => v
      case _ => fail(s"Expired after dormancy period $period waiting for termination condition $label")
    }
  }

  /** How frequently to check a termination condition. */
  sealed trait Progress
  final case object Slow extends Progress
  final case object Fast extends Progress

  /** Like Await.ready but return false on timeout, true on completion, throw InterruptedException. */
  def readyOrNot(awaitable: Awaitable[_]): Boolean = Try(Await.ready(awaitable, TestDuration.Standard)).isSuccess

  def withoutATrace[A](body: => A) = NoTrace(body)

  /** To be thrown by test code to check stack depth. */
  case class Probe(depth: Int) extends ControlThrowable

  /** To be called by test code to check stack depth from assertStackSafe. */
  def probeStackSafety[A](): A = throw new Probe(Thread.currentThread.getStackTrace.length)

  def assertStackSafe[A](run1: => A, run2: => A): Unit = {
    var res1 = -1
    var res2 = -1
    def check(f: Int => Unit): Probe => Boolean = {
      case Probe(depth) => f(depth); true
    }
    assertThrown[Probe](check(depth => res1 = depth))(run1)
    assertThrown[Probe](check(depth => res2 = depth))(run2)
    assertEquals(s"Expected equal stack depths, but got $res1 and $res2", res1, res2)
  }
}

object TestDuration {
  import scala.concurrent.duration.{Duration, SECONDS}
  val Standard = Duration(4, SECONDS)
}

/** Run a thunk, collecting uncaught exceptions from any spawned threads. */
class NoTrace[A](body: => A) extends Runnable {

  private val uncaught = new mutable.ListBuffer[(Thread, Throwable)]()

  @volatile private[testkit] var result: Option[A] = None

  def run(): Unit = {
    import AssertUtil.assertZeroNetThreads
    val group = new ThreadGroup("notrace") {
      override def uncaughtException(t: Thread, e: Throwable): Unit = synchronized {
        uncaught += ((t, e))
      }
    }
    try assertZeroNetThreads(group)(body) match {
      case Success(a) => result = Some(a)
      case Failure(e) => synchronized { uncaught += ((Thread.currentThread, e)) }
    }
    finally group.destroy(): @nowarn("cat=deprecation") // deprecated since JDK 16, will be removed
  }

  private[testkit] lazy val errors: List[(Thread, Throwable)] = synchronized(uncaught.toList)

  private def suppress(t: Throwable, other: Throwable): t.type = { t.addSuppressed(other) ; t }

  private final val noError = None: Option[Throwable]

  def asserted: Option[Throwable] =
    errors.collect { case (_, e: AssertionError) => e }
      .foldLeft(noError)((res, e) => res.map(suppress(_, e)).orElse(Some(e)))

  def apply(test: (Option[A], List[(Thread, Throwable)]) => Option[Throwable]) = {
    run()
    test(result, errors).orElse(asserted).foreach(e => throw e)
  }
}
object NoTrace {
  def apply[A](body: => A): NoTrace[A] = new NoTrace(body)
}
