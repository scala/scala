/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.annotation

/**
 * To customize the error message that's emitted when an implicit of type
 * C[T1,..., TN] cannot be found, annotate the class C with @implicitNotFound.
 * Assuming C has type parameters X1,..., XN, the error message will be the
 * result of replacing all occurrences of ${Xi} in the string msg with the
 * string representation of the corresponding type argument Ti. *
 *
 * @author Adriaan Moors
 * @since 2.8.1
 */
final class implicitNotFound(msg: String) extends scala.annotation.StaticAnnotation {}
