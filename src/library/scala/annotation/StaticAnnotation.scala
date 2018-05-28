/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.annotation

/**
 * A base class for static annotations. These are available to the Scala type checker or Scala
 * reflection, even across different compilation units.
 *
 * Annotation classes defined in Scala are not stored in classfiles in a Java-compatible manner
 * and therefore not visible in Java reflection. In order to achieve this, the annotation has to
 * be written in Java.
 *
 *  @author  Martin Odersky
 *  @since   2.4
 */
trait StaticAnnotation extends Annotation
