/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.annotation.unique

/**
 * An annotation that indicates that the annotated parameter is transient.
 * A transient parameter is borrowed in the sense that it is unique but it
 * cannot be consumed.
 *
 * @author Philipp Haller
 * @since 2.9
 */
final class transient extends annotation.StaticAnnotation
