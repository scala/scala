/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.immutable

import scala.collection.generic.MapProxyTemplate

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
 */

trait MapProxy[A, +B] extends Map[A, B] with MapProxyTemplate[A, B, Map[A, B]]
{
  override def thisCollection = this
  private def newProxy[B1 >: B](newSelf: Map[A, B1]): MapProxy[A, B1] =
    new MapProxy[A, B1] { val self = newSelf }

  override def empty = newProxy(self.empty)
  override def updated [B1 >: B](key: A, value: B1) = newProxy(self.updated(key, value))

  override def + [B1 >: B](kv: (A, B1)): Map[A, B1] = newProxy(self + kv)
  override def + [B1 >: B](elem1: (A, B1), elem2: (A, B1), elems: (A, B1) *) =
    newProxy(self.+(elem1, elem2, elems: _*))
  override def -(key: A) = newProxy(self - key)
}
