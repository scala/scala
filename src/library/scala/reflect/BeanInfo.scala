/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.reflect

/** <p>
 *    This annotation indicates that a JavaBean-compliant BeanInfo
 *    class should be generated for this annotated Scala class.
 *    A val becomes a read-only property. A var becomes a read-write
 *    property. A def becomes a method.
 *  </p>
 *
 *  @author Ross Judson (rjudson@managedobjects.com)
 */
class BeanInfo extends Annotation
