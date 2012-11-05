/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.annotation

/** A base class for classfile annotations. These are stored as
 *  [[http://docs.oracle.com/javase/7/docs/technotes/guides/language/annotations.html#_top Java annotations]]]
 *  in classfiles.
 *
 *  @author  Martin Odersky
 *  @version 1.1, 2/02/2007
 *  @since 2.4
 */
trait ClassfileAnnotation extends StaticAnnotation
