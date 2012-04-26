/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
// GENERATED CODE: DO NOT EDIT. See scala.Function0 for timestamp.

package scala


/** A function of 1 parameter.
 *
 *  In the following example, the definition of succ is a
 *  shorthand for the anonymous class definition anonfun1:
 *
 *  {{{
 *  object Main extends App {
 *    val succ = (x: Int) => x + 1
 *    val anonfun1 = new Function1[Int, Int] {
 *      def apply(x: Int): Int = x + 1
 *    }
 *    assert(succ(0) == anonfun1(0))
 *  }
 *  }}}
 *
 *  Note that `Function1` does not define a total function, as might
 *  be suggested by the existence of [[scala.PartialFunction]]. The only
 *  distinction between `Function1` and `PartialFunction` is that the
 *  latter can specify inputs which it will not handle.

 */
@annotation.implicitNotFound(msg = "No implicit view available from ${T1} => ${R}.")
trait Function1[@specialized(scala.Int, scala.Long, scala.Float, scala.Double, scala.AnyRef) -T1, @specialized(scala.Unit, scala.Boolean, scala.Int, scala.Float, scala.Long, scala.Double, scala.AnyRef) +R] extends AnyRef { self =>
  /** Apply the body of this function to the argument.
   *  @return   the result of function application.
   */
  def apply(v1: T1): R

  override def toString() = "<function1>"
}
