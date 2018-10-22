package scala.tools
package testing

import org.junit.Assert
import Assert._
import scala.reflect.ClassTag
import scala.runtime.ScalaRunTime.stringOf
import scala.collection.GenIterable
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.tools.nsc.settings.ScalaVersion
import scala.util.Properties.javaSpecVersion
import java.lang.ref._
import java.lang.reflect.{Array => _, _}
import java.util.IdentityHashMap

/** This module contains additional higher-level assert statements
 *  that are ultimately based on junit.Assert primitives.
 */
object AssertUtil {
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

  private[this] val version8 = ScalaVersion("8")

  /** Assert on Java 8, but on later versions, just print if assert would fail. */
  def assert8(b: => Boolean, msg: => Any) =
    if (ScalaVersion(javaSpecVersion) == version8) assert(b, msg)
    else if (!b) println(s"assert not $msg")


  /** Assert no new threads, with some margin for arbitrary threads to exit. */
  def assertZeroNetThreads(body: => Unit): Unit = {
    val beforeCount = Thread.activeCount
    val beforeThreads = new Array[Thread](beforeCount)
    assertEquals("Spurious early thread creation.", beforeCount, Thread.enumerate(beforeThreads))

    body

    val afterCount = {
      var n = 1
      while (Thread.activeCount > beforeCount && n < 5) {
        //println("Wait for quiescence")
        Thread.sleep(250L * n)
        n += 1
      }
      Thread.activeCount
    }
    val afterThreads = new Array[Thread](afterCount)
    assertEquals("Spurious late thread creation.", afterCount, Thread.enumerate(afterThreads))
    val staleThreads = afterThreads.toList.diff(beforeThreads)
    //staleThreads.headOption.foreach(_.getStackTrace.foreach(println))
    assertEquals(staleThreads.mkString("There are stale threads: ",",",""), beforeCount, afterCount)
    assertTrue(staleThreads.mkString("There are stale threads: ",",",""), staleThreads.isEmpty)
  }
}
