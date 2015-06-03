/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
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
 * }
 *  }}}
 *
 *  Note that the difference between `Function1` and [[scala.PartialFunction]]
 *  is that the latter can specify inputs which it will not handle.
 */
@annotation.implicitNotFound(msg = "No implicit view available from ${T1} => ${R}.")
trait Function1[@specialized(scala.Int, scala.Long, scala.Float, scala.Double) -T1, @specialized(scala.Unit, scala.Boolean, scala.Int, scala.Float, scala.Long, scala.Double) +R] extends AnyRef { self =>
  /** Apply the body of this function to the argument.
   *  @return   the result of function application.
   */
  def apply(v1: T1): R

  /** Composes two instances of Function1 in a new Function1, with this function applied last.
   *
   *  @tparam   A   the type to which function `g` can be applied
   *  @param    g   a function A => T1
   *  @return       a new function `f` such that `f(x) == apply(g(x))`
   */
  @annotation.unspecialized def compose[A](g: A => T1): A => R = { x => apply(g(x)) }

  /** Composes two instances of Function1 in a new Function1, with this function applied first.
   *
   *  @tparam   A   the result type of function `g`
   *  @param    g   a function R => A
   *  @return       a new function `f` such that `f(x) == g(apply(x))`
   */
  @annotation.unspecialized def andThen[A](g: R => A): T1 => A = { x => g(apply(x)) }

  override def toString() = "<function1>"
}
