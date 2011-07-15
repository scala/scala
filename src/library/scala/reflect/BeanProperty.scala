/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.reflect

import annotation.target._

/** When attached to a field, this annotation adds a setter and a getter
 *  method following the Java Bean convention. For example:
 *  {{{
 *    @BeanProperty
 *    var status = ""
 *  }}}
 *  adds the following methods to the class:
 *  {{{
 *    def setStatus(s: String) { this.status = s }
 *    def getStatus: String = this.status
 *  }}}
 *  For fields of type `Boolean`, if you need a getter named `isStatus`,
 *  use the `scala.reflect.BooleanBeanProperty` annotation instead.
 */
@field
class BeanProperty extends annotation.StaticAnnotation
