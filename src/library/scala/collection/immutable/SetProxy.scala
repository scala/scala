/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala
package collection
package immutable

/** This is a simple wrapper class for [[scala.collection.immutable.Set]].
 *
 *  It is most useful for assembling customized set abstractions
 *  dynamically using object composition and forwarding.
 *
 *  @tparam A    type of the elements contained in this set proxy.
 *
 *  @since 2.8
 */
@deprecated("proxying is deprecated due to lack of use and compiler-level support.", "2.11.0")
trait SetProxy[A] extends Set[A] with SetProxyLike[A, Set[A]] {
  override def repr = this
  private def newProxy[B >: A](newSelf: Set[B]): SetProxy[B] =
    new AbstractSet[B] with SetProxy[B] { val self = newSelf }

  override def empty = newProxy(self.empty)
  override def + (elem: A) = newProxy(self + elem)
  override def - (elem: A) = newProxy(self - elem)
}
