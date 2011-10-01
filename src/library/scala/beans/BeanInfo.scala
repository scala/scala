/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.beans

/** This annotation indicates that a JavaBean-compliant `BeanInfo` class
 *  should be generated for this annotated Scala class.
 *
 *  - A `'''val'''` becomes a read-only property.
 *  - A `'''var'''` becomes a read-write property.
 *  - A `'''def'''` becomes a method.
 *
 *  @author Ross Judson (rjudson@managedobjects.com)
 */
class BeanInfo extends annotation.Annotation
