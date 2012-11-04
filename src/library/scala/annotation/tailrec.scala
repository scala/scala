/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.annotation

/** A method annotation which verifies that the method will be compiled
 *  with tail call optimization.
 *
 *  If it is present, the compiler will issue an error if the method cannot
 *  be optimized into a loop.
 *
 *  @since 2.8
 */
final class tailrec extends scala.annotation.StaticAnnotation
