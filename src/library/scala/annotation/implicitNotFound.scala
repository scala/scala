/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.annotation

/**
 * An annotation that specifies the error message that is emitted when the compiler
 * cannot find an implicit value of the annotated type.
 *
 * @author Adriaan Moors
 * @since 2.8.1
 */
final class implicitNotFound(msg: String) extends scala.annotation.StaticAnnotation {}
