/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.annotation

/** A base class for static annotations.
 *
 *  These are available to the Scala type checker, even across different compilation units.
 *
 *  They are not visible from Java, neither for the Java compiler nor at runtime.
 *
 *  @author  Martin Odersky
 *  @version 1.1, 2/02/2007
 *  @since   2.4
 */
abstract class StaticAnnotation extends Annotation
