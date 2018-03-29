/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2016, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.annotation

/** A type annotation which verifies that the implicit values of the
 *  annotated type will be resolved inductively.
 *
 *  If it is present, the compiler will issue an error if implicit values
 *  cannot be resolved inductively.
 *
 *  @since 2.12.0
 */
final class inductive extends scala.annotation.StaticAnnotation
