/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.reflect

import annotation.target._

/** <p>
 *    This annotation has the same functionality as
 *    <code>scala.reflect.BeanProperty</code>, but the generated
 *    Bean getter will be named <code>isFieldName</code> instead
 *    of <code>getFieldName</code>.
 *  </p>
 */
@field
class BooleanBeanProperty extends annotation.StaticAnnotation
