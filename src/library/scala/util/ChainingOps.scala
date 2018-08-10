package scala
package util

trait ChainingSyntax {
  implicit final def scalaUtilChainingOps[A](a: A): ChainingOps[A] = new ChainingOps(a)
}

/** Adds chaining methods `tap` and `pipe` to every type.
 */
final class ChainingOps[A](private val self: A) extends AnyVal {
  /** Applies `f` to the value for its side effects, and returns the original value.
   *
   *    {{{
   *    val xs = List(1, 2, 3)
   *               .tap(ys => println("debug " + ys.toString))
   *    // xs == List(1, 2, 3)
   *    }}}
   *
   *  @param f      the function to apply to the value.
   *  @tparam U     the result type of the function `f`.
   *  @return       the original value `self`.
   */
  def tap[U](f: A => U): A = {
    f(self)
    self
  }

  /** Converts the value by applying the function `f`.
   *
   *    {{{
   *    val times6 = (_: Int) * 6
   *    val i = (1 - 2 - 3).pipe(times6).pipe(scala.math.abs)
   *    // i == 24
   *    }}}
   *
   * Note: `(1 - 2 - 3).pipe(times6)` may have a small amount of overhead at
   * runtime compared to the equivalent  `{ val temp = 1 - 2 - 3; times6(temp) }`.
   *
   *  @param f      the function to apply to the value.
   *  @tparam B     the result type of the function `f`.
   *  @return       a new value resulting from applying the given function
   *                `f` to this value.
   */
  def pipe[B](f: A => B): B = f(self)
}
