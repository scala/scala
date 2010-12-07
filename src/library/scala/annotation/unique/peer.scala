/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.annotation.unique

/**
 * An annotation that indicates that the annotated parameter is in the
 * same region as the parameter that is the argument of the annotation.
 *
 * @author Philipp Haller
 * @since 2.9
 */
final class peer(x: AnyRef) extends annotation.StaticAnnotation
