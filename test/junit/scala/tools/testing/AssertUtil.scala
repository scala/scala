package scala.tools
package testing

import org.junit.Assert
import Assert._
import scala.runtime.ScalaRunTime.stringOf
import scala.collection.{ GenIterable, IterableLike }
import scala.collection.JavaConverters._
import scala.collection.mutable
import java.lang.ref._
import java.lang.reflect._
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
  def assertThrows[T <: Throwable](body: => Any,
                                   checkMessage: String => Boolean = s => true)
                                  (implicit manifest: Manifest[T]): Unit = {
    try {
      body
      fail("Expression did not throw!")
    } catch {
      case e: Throwable if (manifest.runtimeClass isAssignableFrom e.getClass) &&
                            checkMessage(e.getMessage) =>
    }
  }

  /** JUnit-style assertion for `IterableLike.sameElements`.
   */
  def assertSameElements[A, B >: A](expected: IterableLike[A, _], actual: GenIterable[B], message: String = ""): Unit =
    if (!(expected sameElements actual))
      fail(
        f"${ if (message.nonEmpty) s"$message " else "" }expected:<${ stringOf(expected) }> but was:<${ stringOf(actual) }>"
      )

  /** Convenient for testing iterators.
   */
  def assertSameElements[A, B >: A](expected: IterableLike[A, _], actual: Iterator[B]): Unit =
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
}
