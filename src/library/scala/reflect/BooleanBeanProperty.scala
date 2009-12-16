/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.reflect

/** <p>
 *    This annotation has the same functionality as
 *    <code>scala.reflect.BeanProperty</code>, but the generated
 *    Bean getter will be named <code>isFieldName</code> instead
 *    of <code>getFieldName</code>.
 *  </p>
 */
class BooleanBeanProperty extends StaticAnnotation
