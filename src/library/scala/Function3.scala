/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
// GENERATED CODE: DO NOT EDIT. See scala.Function0 for timestamp.

package scala


/** A function of 3 parameters.
 *
 */
trait Function3[-T1, -T2, -T3, +R] extends AnyRef { self =>
  /** Apply the body of this function to the arguments.
   *  @return   the result of function application.
   */
  def apply(v1: T1, v2: T2, v3: T3): R
  /** Creates a curried version of this function.
   *
   *  @return   a function `f` such that `f(x1)(x2)(x3) == apply(x1, x2, x3)`
   */
  @annotation.unspecialized def curried: T1 => T2 => T3 => R = {
    (x1: T1) => (x2: T2) => (x3: T3) => apply(x1, x2, x3)
  }
  /** Creates a tupled version of this function: instead of 3 arguments,
   *  it accepts a single [[scala.Tuple3]] argument.
   *
   *  @return   a function `f` such that `f((x1, x2, x3)) == f(Tuple3(x1, x2, x3)) == apply(x1, x2, x3)`
   */

  @annotation.unspecialized def tupled: Tuple3[T1, T2, T3] => R = {
    case Tuple3(x1, x2, x3) => apply(x1, x2, x3)
  }
  override def toString() = "<function3>"
}
