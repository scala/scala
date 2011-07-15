/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala

/** An annotation that gets applied to a selector in a match expression.
 *  If it is present, exhaustiveness warnings for that expression will be
 *  suppressed.
 *  For example, compiling the code:
 *  {{{
 *    object test extends Application {
 *      def f(x: Option[Int]) = x match {
 *        case Some(y) => y
 *      }
 *      f(None)
 *    }
 *  }}}
 *  will display the following warning:
 *  {{{
 *    test.scala:2: warning: does not cover case {object None}
 *      def f(x: Option[int]) = x match {
 *                              ^
 *    one warning found
 *  }}}
 *  The above message may be suppressed by substituting the expression `x`
 *  with `(x: @unchecked)`. Then the modified code will compile silently,
 *  but, in any case, a [[scala.MatchError]] will be raised at runtime.
 *
 *  @since 2.4
 */
class unchecked extends annotation.Annotation {}
