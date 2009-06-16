/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable

import generic.MapProxyTemplate

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

trait MapProxy[A, B] extends mutable.Map[A, B] with MapProxyTemplate[A, B, mutable.Map[A, B]]
{
  override def thisCollection = this
  override def empty: MapProxy[A, B] = new MapProxy[A, B] { val self = MapProxy.this.self.empty }

  override def +(kv: (A, B)) = { self.update(kv._1, kv._2) ; this }
  override def + [B1 >: B] (elem1: (A, B1), elem2: (A, B1), elems: (A, B1) *) =
    { self.+(elem1, elem2, elems: _*) ; this }

  override def -(key: A) = { self.remove(key); this }

  override def += (kv: (A, B)) = { self += kv ; this }
  override def -= (key: A) = { self -= key ; this }
}