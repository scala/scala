/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.annotation

/** A method annotation which instructs the compiler to generate a
 *  Java varargs-style forwarder method for interop. This annotation can
 *  only be applied to methods with repeated parameters.
 *
 *  @since 2.9
 */
final class varargs extends scala.annotation.StaticAnnotation
