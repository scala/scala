/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection


/** A default map which builds a default `immutable.Map` implementation for all
  * transformations.
  *
  *  @since 2.8
  */
@deprecated("DefaultMap is no longer necessary; extend Map directly", "2.13.0")
trait DefaultMap[K, +V] extends Map[K, V]
