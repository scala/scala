/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
package scala.annotation

/** <p>
 *    An annotation to be applied to a match expression.  If present,
 *    the compiler will verify that the match has been compiled to a
 *    tableswitch or lookupswitch, and issue an error if it instead
 *    compiles into a series of conditional expressions.
 *  </p>
 */
final class switch extends StaticAnnotation