/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2008-2018, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package util

/**
 * The `Loan`s apply method implements Java's try-with-resources. You can loan
 * out a resource to a function `f` and the resource will be closed when `f`
 * returns regardless of the way `f` returns.
 *
 * See https://docs.oracle.com/javase/specs/jls/se8/html/jls-14.html#jls-14.20.3
 *
 * In the following example `f` is applied to an input stream if it is
 * non-null. If the input stream is null, then the RuntimeException is thrown.
 * If `f` throws an exception, then the input stream is closed and the
 * exception is rethrown.  If furthermore, closing the input stream throw an
 * exception then that exception is suppressed on the exception thrown by `f`.
 *
 * Example:
 * {{{
 *   import scala.util.Loan
 *   import java.io.InputStream
 *
 *   def withResource[A](resource: String)(f: InputStream => A): A =
 *     Loan(Option(getClass.getResourceAsStream(resource)).getOrElse(throw new RuntimeException("Oh no"))) to f
 * }}}
 */
object Loan {
  class Loan[A <: AutoCloseable](resource: A) {
    def to[B](block: A => B): B = {
      var t: Throwable = null
      try {
        block(resource)
      } catch {
        case x: Throwable =>
          t = x
          throw x
      } finally {
        if (resource != null) {
          if (t != null) {
            try {
              resource.close()
            } catch {
              case y: Throwable =>
                t.addSuppressed(y)
            }
          } else {
            resource.close()
          }
        }
      }
    }
  }

  /*
   * Extend try-with-resource to the n-ary case. Given the program text
   *   `Loan(a, b, c) to { case (a, b, c) => foo }`
   * `a` is evaluated immediately, then `b` is evaluated, and then `c` is
   * evaluated. If `b` throws an exception, then `a` is closed.  Likewise, if
   * `c` throws an exception, then both `b` and `a` is closed in that order.
   * Exceptions originating from closing `a` or `b` are suppressed on the
   * exception from `c`.
   */

  class Loan2[A <: AutoCloseable, B <: AutoCloseable](private[Loan] val p1: Loan[A],
                                                      private[Loan] val p2: Loan[B]) {
    def to[C](block: (A, B) => C): C = p1 to { a => p2 to { b => block(a, b) } }
  }

  class Loan3[A <: AutoCloseable, B <: AutoCloseable, C <: AutoCloseable](
      private[Loan] val p1: Loan[A],
      private[Loan] val p2: Loan[B],
      private[Loan] val p3: Loan[C]) {
    def to[D](block: (A, B, C) => D): D = p1 to { a => p2 to { b => p3 to { c => block(a, b, c) } } }
  }

  class Loan4[A <: AutoCloseable, B <: AutoCloseable, C <: AutoCloseable, D <: AutoCloseable](
      private[Loan] val p1: Loan[A],
      private[Loan] val p2: Loan[B],
      private[Loan] val p3: Loan[C],
      private[Loan] val p4: Loan[D]) {
    def to[T](block: (A, B, C, D) => T): T = p1 to { a => p2 to { b => p3 to { c => p4 to { d => block(a, b, c, d) } } } }
  }

  def apply[A <: AutoCloseable](resource: A): Loan[A] = new Loan(resource)

  // b is call-by-name such that a can be closed if b throws an exception
  def apply[A <: AutoCloseable, B <: AutoCloseable](a: A, b: => B): Loan2[A, B] = {
    val loanA = new Loan(a)
    // evaluate b and close a if b throws an exception exn. If a.close() throws an exception exn', then exn' is suppressed on exn
    val loanB = Try(Loan(b)).orValue(exn => loanA to { _ => throw exn })
    new Loan2(loanA, loanB)
  }

  def apply[A <: AutoCloseable, B <: AutoCloseable, C <: AutoCloseable](a: A, b: => B, c: => C): Loan3[A, B, C] = {
    val loanA = new Loan(a)
    val loanBC = Try(Loan(b, c)).orValue(exn => loanA to { _ => throw exn })
    new Loan3(loanA, loanBC.p1, loanBC.p2)
  }

  def apply[A <: AutoCloseable, B <: AutoCloseable, C <: AutoCloseable, D <: AutoCloseable](a: A, b: => B, c: => C, d: => D): Loan4[A, B, C, D] = {
    val loanA = new Loan(a)
    val loanBCD = Try(Loan(b, c, d)).orValue(exn => loanA to { _ => throw exn })
    new Loan4(loanA, loanBCD.p1, loanBCD.p2, loanBCD.p3)
  }
}
