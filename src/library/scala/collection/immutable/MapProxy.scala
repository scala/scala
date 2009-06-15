/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.immutable

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

trait MapProxy[A, +B] extends immutable.Map[A, B] with MapProxyTemplate[A, B, immutable.Map[A, B]]
{
  private def newProxy[B1 >: B](newSelf: immutable.Map[A, B1]): MapProxy[A, B1] =
    new MapProxy[A, B1] { val self = newSelf }

  override def empty = newProxy(immutable.Map[A, B]())
  override def updated [B1 >: B](key: A, value: B1) = newProxy(self.updated(key, value))

  override def + [B1 >: B](kv: (A, B1)): Map[A, B1] = newProxy(self + kv)
  override def + [B1 >: B] (elem1: (A, B1), elem2: (A, B1), elems: (A, B1) *) =
    newProxy(self.+(elem1, elem2, elems: _*))
  override def -(key: A) = newProxy(self - key)

  // error: method ++ overrides nothing  -- ??
  // override def ++[B1 >: B](elems: Traversable[(A, B1)]) = super.++(elems)
  // override def ++[B1 >: B] (iter: Iterator[(A, B1)]): immutable.Map[A, B1] =
  override def filterNot(p: ((A, B)) => Boolean) = newProxy(self.filterNot(p))
  override def update[B1 >: B](key: A, value: B1) = newProxy(self.update(key, value))
}
