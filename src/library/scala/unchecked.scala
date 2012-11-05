/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

/** An annotation to designate that the annotated entity
 *  should not be considered for additional compiler checks.
 *  Specific applications include annotating the subject of
 *  a match expression to suppress exhaustiveness warnings, and
 *  annotating a type argument in a match case to suppress
 *  unchecked warnings.
 *
 *  Such suppression should be used with caution, without which
 *  one may encounter [[scala.MatchError]] or [[java.lang.ClassCastException]]
 *  at runtime.  In most cases one can and should address the
 *  warning instead of suppressing it.
 *
 *  {{{
 *    object Test extends App {
 *      // This would normally warn "match is not exhaustive"
 *      // because `None` is not covered.
 *      def f(x: Option[String]) = (x: @unchecked) match { case Some(y) => y }
 *      // This would normally warn "type pattern is unchecked"
 *      // but here will blindly cast the head element to String.
 *      def g(xs: Any) = xs match { case x: List[String @unchecked] => x.head }
 *    }
 * }}}
 *
 *  @since 2.4
 */
class unchecked extends scala.annotation.Annotation {}
