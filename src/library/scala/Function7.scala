/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
// GENERATED CODE: DO NOT EDIT. See scala.Function0 for timestamp.

package scala


/** A function of 7 parameters.
 *
 */
trait Function7[-T1, -T2, -T3, -T4, -T5, -T6, -T7, +R] extends AnyRef { self =>
  /** Apply the body of this function to the arguments.
   *  @return   the result of function application.
   */
  def apply(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7): R
  /** Creates a curried version of this function.
   *
   *  @return   a function `f` such that `f(x1)(x2)(x3)(x4)(x5)(x6)(x7) == apply(x1, x2, x3, x4, x5, x6, x7)`
   */
  @annotation.unspecialized def curried: T1 => T2 => T3 => T4 => T5 => T6 => T7 => R = {
    (x1: T1) => ((x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7) => self.apply(x1, x2, x3, x4, x5, x6, x7)).curried
  }
  /** Creates a tupled version of this function: instead of 7 arguments,
   *  it accepts a single [[scala.Tuple7]] argument.
   *
   *  @return   a function `f` such that `f((x1, x2, x3, x4, x5, x6, x7)) == f(Tuple7(x1, x2, x3, x4, x5, x6, x7)) == apply(x1, x2, x3, x4, x5, x6, x7)`
   */

  @annotation.unspecialized def tupled: Tuple7[T1, T2, T3, T4, T5, T6, T7] => R = {
    case Tuple7(x1, x2, x3, x4, x5, x6, x7) => apply(x1, x2, x3, x4, x5, x6, x7)
  }
  override def toString() = "<function7>"
}
