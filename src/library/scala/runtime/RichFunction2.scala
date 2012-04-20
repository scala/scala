/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.runtime

@inline final class RichFunction2[-T1, -T2, +R](f: (T1, T2) => R) {
  /** Creates a curried version of this function.
   *
   *  @return   a function `f` such that `f(x1)(x2) == apply(x1, x2)`
   */
  def curried: T1 => T2 => R = (x1: T1) => (x2: T2) => f(x1, x2)

  /** Creates a tupled version of this function: instead of 2 arguments,
   *  it accepts a single [[scala.Tuple2]] argument.
   *
   *  @return   a function `f` such that `f((x1, x2)) == f(Tuple2(x1, x2)) == apply(x1, x2)`
   */
  def tupled: ((T1, T2)) => R = {
    case Tuple2(x1, x2) => f(x1, x2)
  }
}
