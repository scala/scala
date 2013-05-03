/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection
package generic

/** This trait forms part of collections that can be cleared
 *  with a clear() call.
 *
 *  @author   Paul Phillips
 *  @version 2.10
 *  @since   2.10
 *  @define coll clearable collection
 *  @define Coll `Clearable`
 */
trait Clearable {
  /** Clears the $coll's contents. After this operation, the
   *  $coll is empty.
   */
  def clear(): Unit
}
