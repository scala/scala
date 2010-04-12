/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package immutable

import generic._

/** The canonical factory of <a href="ListMap.html">ListMap</a>'s.
 *
 *  @since 1
 */
object ListMap extends ImmutableMapFactory[ListMap] {
  implicit def canBuildFrom[A, B]: CanBuildFrom[Coll, (A, B), ListMap[A, B]] =
    new MapCanBuildFrom[A, B]
  def empty[A, B]: ListMap[A, B] = new ListMap
}

/** This class implements immutable maps using a list-based data
 *  structure. Instances of <code>ListMap</code> represent
 *  empty maps; they can be either created by calling the constructor
 *  directly, or by applying the function <code>ListMap.empty</code>.
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.0, 01/01/2007
 *  @since   1
 */
@serializable @SerialVersionUID(301002838095710379L)
class ListMap[A, +B] extends Map[A, B] with MapLike[A, B, ListMap[A, B]] {

  override def empty = ListMap.empty

  /** Returns the number of mappings in this map.
   *
   *  @return number of mappings in this map.
   */
  override def size: Int = 0

  /** Checks if this map maps <code>key</code> to a value and return the
   *  value if it exists.
   *
   *  @param  key the key of the mapping of interest
   *  @return     the value of the mapping, if it exists
   */
  def get(key: A): Option[B] = None

  /** This method allows one to create a new map with an
   *  additional mapping from <code>key</code>
   *  to <code>value</code>. If the map contains already a
   *  mapping for <code>key</code>, it will be overridden by this
   *  function.
   *
   *  @param key  the key element of the updated entry.
   *  @param value the value element of the updated entry.
   */
  override def updated [B1 >: B] (key: A, value: B1): ListMap[A, B1] = new Node[B1](key, value)

  /** Add a key/value pair to this map.
   *  @param    kv the key/value pair
   *  @return   A new map with the new binding added to this map
   */
  def + [B1 >: B] (kv: (A, B1)): ListMap[A, B1] = updated(kv._1, kv._2)

  /** Adds two or more elements to this collection and returns
   *  either the collection itself (if it is mutable), or a new collection
   *  with the added elements.
   *
   *  @param elem1 the first element to add.
   *  @param elem2 the second element to add.
   *  @param elems the remaining elements to add.
   */
  override def + [B1 >: B] (elem1: (A, B1), elem2: (A, B1), elems: (A, B1) *): ListMap[A, B1] =
    this + elem1 + elem2 ++ elems

  /** This creates a new mapping without the given <code>key</code>.
   *  If the map does not contain a mapping for the given key, the
   *  method returns the same map.
   *
   *  @param key a map without a mapping for the given key.
   */
  def - (key: A): ListMap[A, B] = this

  /** Returns an iterator over key-value pairs.
   */
  def iterator: Iterator[(A,B)] =
    new Iterator[(A,B)] {
      var self: ListMap[A,B] = ListMap.this
      def hasNext = !self.isEmpty
      def next: (A,B) =
        if (!hasNext) throw new NoSuchElementException("next on empty iterator")
        else { val res = (self.key, self.value); self = self.next; res }
    }.toList.reverseIterator

  protected def key: A = throw new NoSuchElementException("empty map")
  protected def value: B = throw new NoSuchElementException("empty map")
  protected def next: ListMap[A, B] = throw new NoSuchElementException("empty map")

  /** This class represents an entry in the `ListMap`.
   */
  @serializable @SerialVersionUID(-6453056603889598734L)
  protected class Node[B1 >: B](override protected val key: A,
                                override protected val value: B1) extends ListMap[A, B1] {
    /** Returns the number of mappings in this map.
     *
     *  @return number of mappings.
     */
    override def size: Int = next.size + 1

    /** Is this an empty map?
     *
     *  @return true, iff the map is empty.
     */
    override def isEmpty: Boolean = false

    /** Retrieves the value which is associated with the given key. This
     *  method throws an exception if there is no mapping from the given
     *  key to a value.
     *
     *  @param  key the key
     *  @return     the value associated with the given key.
     */
    override def apply(k: A): B1 = if (k == key) value else next(k)

    /** Checks if this map maps <code>key</code> to a value and return the
     *  value if it exists.
     *
     *  @param  key the key of the mapping of interest
     *  @return     the value of the mapping, if it exists
     */
    override def get(k: A): Option[B1] =
      if (k == key) Some(value) else next.get(k)

    /** This method allows one to create a new map with an
     *  additional mapping from <code>key</code>
     *  to <code>value</code>. If the map contains already a
     *  mapping for <code>key</code>, it will be overridden by this
     *  function.
     *
     *  @param k ...
     *  @param v ...
     */
    override def updated [B2 >: B1](k: A, v: B2): ListMap[A, B2] = {
      val m = if (contains(k)) this - k else this
      new m.Node[B2](k, v)
    }

    /** Creates a new mapping without the given <code>key</code>.
     *  If the map does not contain a mapping for the given key, the
     *  method returns the same map.
     *
     *  @param k ...
     *  @return  ...
     */
    override def - (k: A): ListMap[A, B1] =
      if (k == key)
        next
      else {
        val tail = next - k
        if (tail eq next) this
        else new tail.Node(key, value)
      }

    override protected def next: ListMap[A, B1] = ListMap.this
  }
}
