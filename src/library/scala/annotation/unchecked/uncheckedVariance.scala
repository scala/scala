/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
package scala.annotation.unchecked

/** An annotation for type arguments for which one wants to suppress variance checking
 *  types are volatile.
 *
 *  @since 2.7
 */
final class uncheckedVariance extends scala.annotation.StaticAnnotation {}
