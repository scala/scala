/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package xml
package include

/**
 * An `UnavailableResourceException` is thrown when an included document
 * cannot be found or loaded.
 */
class UnavailableResourceException(message: String)
extends XIncludeException(message) {
  def this() = this(null)
}
