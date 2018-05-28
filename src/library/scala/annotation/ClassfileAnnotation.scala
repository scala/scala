/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.annotation

/** A base class for classfile annotations. These are stored as
 *  [[http://docs.oracle.com/javase/8/docs/technotes/guides/language/annotations.html Java annotations]]]
 *  in classfiles.
 *
 *  @author  Martin Odersky
 *  @since 2.4
 */
@deprecated("Annotation classes need to be written in Java in order to be stored in classfiles in a Java-compatible manner", "2.13.0")
trait ClassfileAnnotation extends ConstantAnnotation
