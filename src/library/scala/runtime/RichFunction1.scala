/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.runtime

@inline final class RichFunction1[-T1, +R](f: T1 => R) {
  /** Composes two instances of Function1 in a new Function1, with the function on the left applied last.
   *
   *  @tparam   A   the type to which function `g` can be applied
   *  @param    g   a function A => T1
   *  @return       a new function `h` such that `h(x) == f(g(x))`
   */
  def compose[A](g: A => T1): A => R = { x => f(g(x)) }

  /** Composes two instances of Function1 in a new Function1, with the function on the left applied first.
   *
   *  @tparam   A   the result type of function `g`
   *  @param    g   a function R => A
   *  @return       a new function `h` such that `h(x) == g(f(x))`
   */
  def andThen[A](g: R => A): T1 => A = { x => g(f(x)) }
}
