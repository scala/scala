/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.beans

/** This annotation has the same functionality as
 *  `scala.beans.BeanProperty`, but the generated Bean getter will be
 *  named `isFieldName` instead of `getFieldName`.
 */
@scala.annotation.meta.field
class BooleanBeanProperty extends annotation.StaticAnnotation
