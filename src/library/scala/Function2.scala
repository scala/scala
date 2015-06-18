/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
// GENERATED CODE: DO NOT EDIT. See scala.Function0 for timestamp.

package scala


/** A function of 2 parameters.
 *  
 *  In the following example, the definition of max is a
 *  shorthand for the anonymous class definition anonfun2:
 *
 *  {{{
 *  object Main extends App {
 *    val max = (x: Int, y: Int) => if (x < y) y else x
 *
 *    val anonfun2 = new Function2[Int, Int, Int] {
 *      def apply(x: Int, y: Int): Int = if (x < y) y else x
 *    }
 *    assert(max(0, 1) == anonfun2(0, 1))
 * }
 *  }}}
 */
trait Function2[@specialized(scala.Int, scala.Long, scala.Double) -T1, @specialized(scala.Int, scala.Long, scala.Double) -T2, @specialized(scala.Unit, scala.Boolean, scala.Int, scala.Float, scala.Long, scala.Double) +R] extends AnyRef { self =>
  /** Apply the body of this function to the arguments.
   *  @return   the result of function application.
   */
  def apply(v1: T1, v2: T2): R
  /** Creates a curried version of this function.
   *
   *  @return   a function `f` such that `f(x1)(x2) == apply(x1, x2)`
   */
  @annotation.unspecialized def curried: T1 => T2 => R = {
    (x1: T1) => (x2: T2) => apply(x1, x2)
  }
  /** Creates a tupled version of this function: instead of 2 arguments,
   *  it accepts a single [[scala.Tuple2]] argument.
   *
   *  @return   a function `f` such that `f((x1, x2)) == f(Tuple2(x1, x2)) == apply(x1, x2)`
   */

  @annotation.unspecialized def tupled: Tuple2[T1, T2] => R = {
    case Tuple2(x1, x2) => apply(x1, x2)
  }
  override def toString() = "<function2>"
}
