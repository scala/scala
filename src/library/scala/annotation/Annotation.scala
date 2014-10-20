/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.annotation

/** A base class for annotations.
 *
 *  Annotations extending this class directly are not preserved for the Scala
 *  type checker and are also not stored as Java annotations in classfiles.
 *
 *  To make an annotation visible to the Scala type checker, it needs to
 *  inherit from [[scala.annotation.StaticAnnotation]].
 *
 *  To persist an annotation as a Java annotation in classfiles, it needs
 *  to inherit from either [[scala.annotation.ClassfileAnnotation]]
 *  (annotation not visible at runtime) or
 *  [[scala.annotation.RuntimeAnnotation]](annotation visible at runtime).
 *
 *  @author  Martin Odersky
 *  @version 1.1, 2/02/2007
 *  @since 2.4
 */
trait Annotation
