/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.annotation

/** A base class for static annotations. These are available
 *  to the Scala type checker, even across different compilation units.
 *
 *  @author  Martin Odersky
 *  @version 1.1, 2/02/2007
 *  @since   2.4
 */
trait StaticAnnotation extends Annotation
