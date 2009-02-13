/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Map.scala 16884 2009-01-09 16:52:09Z cunei $


package scalax.collection.mutable

import generic._

/* Factory object for `Map` class */
object Map extends MapFactory[Map] {
  def empty[A, B]: Map[A, B] = new HashMap[A, B]
}

trait Map[A, B]
  extends collection.Map[A, B]
     with MapTemplate[A, B, Map]
     with Growable[(A, B)]
     with Shrinkable[A]
     with Cloneable[Map[A, B]] {
self =>

  override def thisCC: Map[A, B] = this

  /** This method allows one to add a new mapping from <code>key</code>
   *  to <code>value</code> to the map. If the map already contains a
   *  mapping for <code>key</code>, it will be overridden by this
   *  function.
   *
   * @param key    The key to update
   * @param value  The new value
   */
  def update(key: A, value: B): this.type

  /** Add a key/value pair to this map.
   *  @param    kv the key/value pair.
   */
  def += (kv: (A, B)) { update(kv._1, kv._2) }

  /** Remove a key from this map, noop if key is not present.
   *  @param    key the key to be removed
   */
  def -= (key: A)

  def -(key: A): this.type = { -=(key); this }

  /** Removes all elements from the set for
   *  which the predicate <code>p</code> yields the value <code>false</code>.
   */
  def retain(p: A => Boolean): Unit = for ((k, v) <- this) if (!p(k)) -=(k)

  /** Removes all elements from the set. After this operation is completed,
   *  the set will be empty.
   */
  def clear() { for ((k, v) <- elements) -=(k) }

  override def clone(): Map[A, B] = empty[B] ++ this

  /** Return a read-only projection of this set !!! just us an (immutable) setProxy? */
  def readOnly : collection.Map[A, B] = new collection.Map[A, B] {
    override def size = self.size
    override def update(key: A, value: B) = self.update(key, value)
    override def - (elem: A) = self - elem
    override def elements = self.elements
    override def foreach(f: ((A, B)) => Unit) = self.foreach(f)
    override def empty[C] = self.empty[C]
    def get(key: A) = self.get(key)
  }
}
