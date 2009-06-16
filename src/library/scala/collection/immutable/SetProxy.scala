/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection.immutable

import generic.SetProxyTemplate

/** <p>
 *    This is a simple wrapper class for <a href="Set.html"
 *    target="contentFrame"><code>scala.collection.immutable.Set</code></a>.
 *  </p>
 *  <p>
 *    It is most useful for assembling customized set abstractions
 *    dynamically using object composition and forwarding.
 *  </p>
 */

trait SetProxy[A] extends immutable.Set[A] with SetProxyTemplate[A, immutable.Set[A]]
{
  override def thisCollection = this
  private def newProxy[B >: A](newSelf: immutable.Set[B]): SetProxy[B] =
    new SetProxy[B] { val self = newSelf }

  override def empty = newProxy(self.empty)
  override def + (elem: A) = newProxy(self + elem)
  override def - (elem: A) = newProxy(self - elem)
}
