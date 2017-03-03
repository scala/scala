/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection
package mutable

/** This is a simple wrapper class for [[scala.collection.mutable.Set]].
 *  It is most useful for assembling customized set abstractions
 *  dynamically using object composition and forwarding.
 *
 *  @author  Matthias Zenger
 *  @version 1.1, 09/05/2004
 *  @since   1
 */
@deprecated("proxying is deprecated due to lack of use and compiler-level support", "2.11.0")
trait SetProxy[A] extends Set[A] with SetProxyLike[A, Set[A]] {
  override def repr = this
  override def empty = new SetProxy[A] { val self = SetProxy.this.self.empty }
  override def + (elem: A) = { self += elem ; this }
  override def - (elem: A) = { self -= elem ; this }

  def +=(elem: A) = { self += elem; this }
  def -=(elem: A) = { self -= elem; this }
}
