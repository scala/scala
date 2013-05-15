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

import generic._
import scala.annotation.{tailrec, bridge}

/** $factoryInfo
 *  @since 1
 *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-immutable-collection-classes.html#list_maps "Scala's Collection Library overview"]]
 *  section on `List Maps` for more information.
 *
 *  @define Coll immutable.ListMap
 *  @define coll immutable list map
 */
object ListMap extends ImmutableMapFactory[ListMap] {
  /** $mapCanBuildFromInfo */
  implicit def canBuildFrom[A, B]: CanBuildFrom[Coll, (A, B), ListMap[A, B]] =
    new MapCanBuildFrom[A, B]
  def empty[A, B]: ListMap[A, B] = EmptyListMap.asInstanceOf[ListMap[A, B]]

  private object EmptyListMap extends ListMap[Any, Nothing] { }
}

/** This class implements immutable maps using a list-based data structure.
 *  Instances of `ListMap` represent empty maps; they can be either created by
 *  calling the constructor directly, or by applying the function `ListMap.empty`.
 *
 *  @tparam A     the type of the keys in this list map.
 *  @tparam B     the type of the values associated with the keys.
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.0, 01/01/2007
 *  @since   1
 *  @define Coll immutable.ListMap
 *  @define coll immutable list map
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
@SerialVersionUID(301002838095710379L)
class ListMap[A, +B]
extends AbstractMap[A, B]
   with Map[A, B]
   with MapLike[A, B, ListMap[A, B]]
   with Serializable {

  override def empty = ListMap.empty

  /** Returns the number of mappings in this map.
   *
   *  @return number of mappings in this map.
   */
  override def size: Int = 0

  /** Checks if this map maps `key` to a value and return the
   *  value if it exists.
   *
   *  @param  key the key of the mapping of interest
   *  @return     the value of the mapping, if it exists
   */
  def get(key: A): Option[B] = None

  /** This method allows one to create a new map with an additional mapping
   *  from `key` to `value`. If the map contains already a mapping for `key`,
   *  it will be overridden by this function.
   *
   *  @param key  the key element of the updated entry.
   *  @param value the value element of the updated entry.
   */
  override def updated [B1 >: B] (key: A, value: B1): ListMap[A, B1] =
    new Node[B1](key, value)

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

  /** Adds a number of elements provided by a traversable object
   *  and returns a new collection with the added elements.
   *
   *  @param xs     the traversable object.
   */
  override def ++[B1 >: B](xs: GenTraversableOnce[(A, B1)]): ListMap[A, B1] =
    ((repr: ListMap[A, B1]) /: xs.seq) (_ + _)

  /** This creates a new mapping without the given `key`.
   *  If the map does not contain a mapping for the given key, the
   *  method returns the same map.
   *
   *  @param key a map without a mapping for the given key.
   */
  def - (key: A): ListMap[A, B] = this

  /** Returns an iterator over key-value pairs.
   */
  def iterator: Iterator[(A,B)] =
    new AbstractIterator[(A,B)] {
      var self: ListMap[A,B] = ListMap.this
      def hasNext = !self.isEmpty
      def next(): (A,B) =
        if (!hasNext) throw new NoSuchElementException("next on empty iterator")
        else { val res = (self.key, self.value); self = self.tail; res }
    }.toList.iterator

  protected def key: A = throw new NoSuchElementException("empty map")
  protected def value: B = throw new NoSuchElementException("empty map")
  override def tail: ListMap[A, B] = throw new NoSuchElementException("empty map")

  /** This class represents an entry in the `ListMap`.
   */
  @SerialVersionUID(-6453056603889598734L)
  protected class Node[B1 >: B](override protected val key: A,
                                override protected val value: B1) extends ListMap[A, B1] with Serializable {
    /** Returns the number of mappings in this map.
     *
     *  @return number of mappings.
     */
    override def size: Int = size0(this, 0)

    // to allow tail recursion and prevent stack overflows
    @tailrec private def size0(cur: ListMap[A, B1], acc: Int): Int = if (cur.isEmpty) acc else size0(cur.tail, acc + 1)

    /** Is this an empty map?
     *
     *  @return true, iff the map is empty.
     */
    override def isEmpty: Boolean = false

    /** Retrieves the value which is associated with the given key. This
     *  method throws an exception if there is no mapping from the given
     *  key to a value.
     *
     *  @param  k   the key
     *  @return     the value associated with the given key.
     */
    override def apply(k: A): B1 = apply0(this, k)
 
    
    @tailrec private def apply0(cur: ListMap[A, B1], k: A): B1 = 
      if (cur.isEmpty) throw new NoSuchElementException("key not found: "+k)
      else if (k == cur.key) cur.value
      else apply0(cur.tail, k)  

    /** Checks if this map maps `key` to a value and return the
     *  value if it exists.
     *
     *  @param  k   the key of the mapping of interest
     *  @return     the value of the mapping, if it exists
     */
    override def get(k: A): Option[B1] = get0(this, k)

    @tailrec private def get0(cur: ListMap[A, B1], k: A): Option[B1] =
      if (k == cur.key) Some(cur.value)
      else if (cur.tail.nonEmpty) get0(cur.tail, k) else None

    /** This method allows one to create a new map with an additional mapping
     *  from `key` to `value`. If the map contains already a mapping for `key`,
     *  it will be overridden by this function.
     */
    override def updated [B2 >: B1](k: A, v: B2): ListMap[A, B2] = {
      val m = if (contains(k)) this - k else this
      new m.Node[B2](k, v)
    }

    /** Creates a new mapping without the given `key`.
     *  If the map does not contain a mapping for the given key, the
     *  method returns the same map.
     */
    override def - (k: A): ListMap[A, B1] = {
      // This definition used to result in stack overflows
      // if (k == key)
      //   next
      // else {
      //   val tail = next - k
      //   if (tail eq next) this
      //   else new tail.Node(key, value)
      // }
      // we use an imperative one instead (and use an auxiliary list to preserve order!):
      var cur: ListMap[A, B1] = this
      var lst: List[(A, B1)] = Nil
      while (cur.nonEmpty) {
        if (k != cur.key) lst ::= ((cur.key, cur.value))
        cur = cur.tail
      }
      var acc = ListMap[A, B1]()
      while (lst != Nil) {
        val elem = lst.head
        val stbl = acc
        acc = new stbl.Node(elem._1, elem._2)
        lst = lst.tail
      }
      acc
    }


    override def tail: ListMap[A, B1] = ListMap.this
    override def head: (A, B1) = (key,value)
  }
}
