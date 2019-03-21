package scala.tools.testkit

import org.junit.Assert, Assert._
import scala.reflect.ClassTag
import scala.runtime.ScalaRunTime.stringOf
import scala.collection.GenIterable
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.concurrent.{Await, Awaitable, SyncVar, TimeoutException}
import scala.util.Try
import scala.util.Properties.isJavaAtLeast
import scala.util.control.NonFatal
import java.lang.ref._
import java.lang.reflect.{Array => _, _}
import java.util.IdentityHashMap

/** This module contains additional higher-level assert statements
 *  that are ultimately based on junit.Assert primitives.
 */
object AssertUtil {

  /** Assert on Java 8, but on later versions, just print if assert would fail. */
  def assert8(b: => Boolean, msg: => Any) =
    if (!isJavaAtLeast("9"))
      assert(b, msg)
    else if (!b)
      println(s"assert not $msg")

  private final val timeout = 60 * 1000L                 // wait a minute

  private implicit class `ref helper`[A](val r: Reference[A]) extends AnyVal {
    def isEmpty: Boolean  = r.get == null
    def nonEmpty: Boolean = !isEmpty
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

  /** Check that throwable T (or a subclass) was thrown during evaluation of `body`,
   *  and that its message satisfies the `checkMessage` predicate.
   *  Any other exception is propagated.
   */
  def assertThrows[T <: Throwable: ClassTag](body: => Any,
      checkMessage: String => Boolean = s => true): Unit = {
    try {
      body
      fail("Expression did not throw!")
    } catch {
      case e: T if checkMessage(e.getMessage) =>
    }
  }

  def assertThrown[T <: Throwable: ClassTag](checker: T => Boolean)(body: => Any): Unit =
    try {
      body
      fail("Expression did not throw!")
    } catch {
      case e: T if checker(e) => ()
      case failed: T =>
        val ae = new AssertionError(s"Exception failed check: $failed")
        ae.addSuppressed(failed)
        throw ae
      case NonFatal(other) =>
        val ae = new AssertionError(s"Exception not a ${implicitly[ClassTag[T]]}: $other")
        ae.addSuppressed(other)
        throw ae
    }

  /** JUnit-style assertion for `IterableLike.sameElements`.
   */
  def assertSameElements[A, B >: A](expected: Iterable[A], actual: Iterable[B], message: String = ""): Unit =
    if (!(expected sameElements actual))
      fail(
        f"${ if (message.nonEmpty) s"$message " else "" }expected:<${ stringOf(expected) }> but was:<${ stringOf(actual) }>"
      )

  /** Convenient for testing iterators.
   */
  def assertSameElements[A, B >: A](expected: Iterable[A], actual: IterableOnce[B]): Unit =
    assertSameElements(expected, actual.toList, "")

  /** Value is not strongly reachable from roots after body is evaluated.
   */
  def assertNotReachable[A <: AnyRef](a: => A, roots: AnyRef*)(body: => Unit): Unit = {
    val wkref = new WeakReference(a)
    def refs(root: AnyRef): mutable.Set[AnyRef] = {
      val seen = new IdentityHashMap[AnyRef, Unit]
      def loop(o: AnyRef): Unit =
        if (wkref.nonEmpty && o != null && !seen.containsKey(o)) {
          seen.put(o, ())
          for {
            f <- o.getClass.allFields
            if !Modifier.isStatic(f.getModifiers)
            if !f.getType.isPrimitive
            if !classOf[Reference[_]].isAssignableFrom(f.getType)
          } loop(f follow o)
        }
      loop(root)
      seen.keySet.asScala
    }
    body
    for (r <- roots if wkref.nonEmpty) {
      assertFalse(s"Root $r held reference", refs(r) contains wkref.get)
    }
  }

  /** Assert no new threads, with some margin for arbitrary threads to exit. */
  def assertZeroNetThreads(body: => Unit): Unit = {
    val result = new SyncVar[Option[Throwable]]
    val group = new ThreadGroup("junit")
    def check() = {
      val beforeCount = group.activeCount
      val beforeThreads = new Array[Thread](beforeCount)
      assertEquals("Spurious early thread creation.", beforeCount, group.enumerate(beforeThreads))

      body

      val afterCount = {
        waitForIt(group.activeCount <= beforeCount, label = "after count")
        group.activeCount
      }
      val afterThreads = new Array[Thread](afterCount)
      assertEquals("Spurious late thread creation.", afterCount, group.enumerate(afterThreads))
      val staleThreads = afterThreads.toList.diff(beforeThreads)
      //staleThreads.headOption.foreach(_.getStackTrace.foreach(println))
      assertEquals(staleThreads.mkString("There are stale threads: ",",",""), beforeCount, afterCount)
      assertTrue(staleThreads.mkString("There are stale threads: ",",",""), staleThreads.isEmpty)
    }
    def test() = {
      try {
        check()
        result.put(None)
      } catch {
        case t: Throwable => result.put(Some(t))
      }
    }
    val timeout = 10 * 1000L  // last chance timeout
    val thread = new Thread(group, () => test())
    def resulted: Boolean = result.get(timeout).isDefined
    try {
      thread.start()
      waitForIt(resulted, Slow, label = "test result")
      val err = result.take(timeout)
      err.foreach(e => throw e)
    } finally {
      thread.join(timeout)
      group.destroy()
    }
  }

  /** Wait for a condition, with a simple back-off strategy.
   *
   *  It would be nicer if what we're waiting for gave us
   *  a progress indicator: we don't care if something
   *  takes a long time, so long as we can verify progress.
   */
  def waitForIt(terminated: => Boolean, progress: Progress = Fast, label: => String = "test"): Unit = {
    val limit = 5
    var n = 1
    var (dormancy, factor) = progress match {
      case Slow => (10000L, 5)
      case Fast => (250L, 4)
    }
    var period = 0L
    var done = false
    var ended = false
    while (!done && n < limit) {
      try {
        ended = terminated
        if (ended) {
          done = true
        } else {
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
    assertTrue(s"Expired after dormancy period $period waiting for termination condition $label", ended)
  }

  /** How frequently to check a termination condition. */
  sealed trait Progress
  final case object Slow extends Progress
  final case object Fast extends Progress

  /** Like Await.ready but return false on timeout, true on completion, throw InterruptedException. */
  def readyOrNot(awaitable: Awaitable[_]): Boolean = Try(Await.ready(awaitable, TestDuration.Standard)).isSuccess
}

object TestDuration {
  import scala.concurrent.duration.{Duration, SECONDS}
  val Standard = Duration(4, SECONDS)
}
