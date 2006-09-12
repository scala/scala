/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection


/** This is a simple wrapper class for <code>scala.collection.Map</code>.
 *  It is most useful for assembling customized map abstractions
 *  dynamically using object composition and forwarding.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 21/07/2003
 */
trait MapProxy[A, +B] extends Map[A, B] with IterableProxy[Pair[A, B]] {

  def self: Map[A, B]

  def size: Int = self.size

  def get(key: A): Option[B] = self.get(key)

  override def isEmpty: Boolean = self.isEmpty

  override def apply(key: A): B = self.apply(key)

  override def contains(key: A): Boolean = self.contains(key)

  override def isDefinedAt(key: A) = self.isDefinedAt(key)

  override def keys: Iterator[A] = self.keys

  override def values: Iterator[B] = self.values

}
