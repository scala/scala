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
}
