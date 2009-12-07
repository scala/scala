/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

import annotation.target._

/**
 * An annotation that designates the definition to which it is applied as deprecated.
 * Access to the member then generates a deprecated warning.
 *
 * @since 2.3
 */
@getter @setter @beanGetter @beanSetter
class deprecated(message: String) extends StaticAnnotation {
  def this() = this("")
}
