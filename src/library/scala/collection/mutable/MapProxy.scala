/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package mutable

/** <p>
 *    This is a simple wrapper class for <a href="Map.html"
 *    target="contentFrame"><code>scala.collection.mutable.Map</code></a>.
 *  </p>
 *  <p>
 *    It is most useful for assembling customized map abstractions
 *    dynamically using object composition and forwarding.
 *  </p>
 *
 *  @author  Matthias Zenger, Martin Odersky
 *  @version 2.0, 31/12/2006
 *  @since   1
 */

trait MapProxy[A, B] extends Map[A, B] with MapProxyLike[A, B, Map[A, B]]
{
  private def newProxy[B1 >: B](newSelf: Map[A, B1]): MapProxy[A, B1] =
    new MapProxy[A, B1] { val self = newSelf }

  override def repr = this
  override def empty: MapProxy[A, B] = new MapProxy[A, B] { val self = MapProxy.this.self.empty }

  override def + [B1 >: B] (kv: (A, B1)): Map[A, B1] = newProxy(self + kv)
  override def + [B1 >: B] (elem1: (A, B1), elem2: (A, B1), elems: (A, B1) *) = newProxy(self.+(elem1, elem2, elems: _*))

  override def -(key: A) = newProxy(self - key)

  override def += (kv: (A, B)) = { self += kv ; this }
  override def -= (key: A) = { self -= key ; this }
}
