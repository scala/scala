/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
// GENERATED CODE: DO NOT EDIT. See scala.Function0 for timestamp.

package scala


/** A function of 6 parameters.
 *
 */
trait Function6[-T1, -T2, -T3, -T4, -T5, -T6, +R] extends AnyRef { self =>
  /** Apply the body of this function to the arguments.
   *  @return   the result of function application.
   */
  def apply(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6): R
  /** Creates a curried version of this function.
   *
   *  @return   a function `f` such that `f(x1)(x2)(x3)(x4)(x5)(x6) == apply(x1, x2, x3, x4, x5, x6)`
   */
  @annotation.unspecialized def curried: T1 => T2 => T3 => T4 => T5 => T6 => R = {
    (x1: T1) => ((x2: T2, x3: T3, x4: T4, x5: T5, x6: T6) => self.apply(x1, x2, x3, x4, x5, x6)).curried
  }
  /** Creates a tupled version of this function: instead of 6 arguments,
   *  it accepts a single [[scala.Tuple6]] argument.
   *
   *  @return   a function `f` such that `f((x1, x2, x3, x4, x5, x6)) == f(Tuple6(x1, x2, x3, x4, x5, x6)) == apply(x1, x2, x3, x4, x5, x6)`
   */

  @annotation.unspecialized def tupled: Tuple6[T1, T2, T3, T4, T5, T6] => R = {
    case Tuple6(x1, x2, x3, x4, x5, x6) => apply(x1, x2, x3, x4, x5, x6)
  }
  override def toString() = "<function6>"
}
