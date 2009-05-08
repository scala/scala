/* TODO: Reintegrate
/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection


/** This is a simple wrapper class for <a href="Map.html"
 *  target="contentFrame"><code>scala.collection.Map</code></a>.
 *  It is most useful for assembling customized map abstractions
 *  dynamically using object composition and forwarding.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 21/07/2003
 */
trait MapProxy[A, +B] extends Map[A, B] with CollectionProxy[(A, B)] {

  override def self: Map[A, B]

  override def size = self.size
  override def get(key: A) = self.get(key)
  override def getOrElse[B2 >: B](key: A, default: => B2) = self.getOrElse(key, default)

  override def isEmpty = self.isEmpty
  override def apply(key: A) = self.apply(key)
  override def contains(key: A) = self.contains(key)
  override def isDefinedAt(key: A) = self.isDefinedAt(key)
  override def keys = self.keys
  override def keySet = self.keySet
  override def values = self.values
  override def equals(that: Any) = self equals that
  override def hashCode() = self.hashCode()
  override def toString() = self.toString()
  override def default(key: A) = self.default(key)
  override def projection = self.projection
  override def filterKeys(p: A => Boolean) = self filterKeys p
  override def mapElements[C](f: B => C) = self mapElements f
}
*/
