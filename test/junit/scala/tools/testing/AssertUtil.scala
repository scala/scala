package scala.tools
package testing

import org.junit.Assert
import Assert.fail
import scala.runtime.ScalaRunTime.stringOf
import scala.collection.{ GenIterable, IterableLike }

/** This module contains additional higher-level assert statements
 *  that are ultimately based on junit.Assert primitives.
 */
object AssertUtil {
  /**
   * Check if throwable T (or a subclass) was thrown during evaluation of f, and that its message
   * satisfies the `checkMessage` predicate.
   * If any other exception will be re-thrown.
   */
  def assertThrows[T <: Throwable](f: => Any,
                                   checkMessage: String => Boolean = s => true)
                                  (implicit manifest: Manifest[T]): Unit = {
    try f
    catch {
      case e: Throwable if checkMessage(e.getMessage) =>
        val clazz = manifest.runtimeClass
        if (!clazz.isAssignableFrom(e.getClass))
          throw e
        else return
    }
    fail("Expression did not throw!")
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
}
