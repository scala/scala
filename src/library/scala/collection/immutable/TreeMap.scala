/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

// todo: make balanced once Tree.scala is updated to be covariant.

package scala.collection.immutable

import generic._

/** The canonical factory of <a href="TreeMap.html">TreeMap</a>'s. */
object TreeMap {

  type Coll = TreeMap[_, _]
  implicit def builderFactory[A <% Ordered[A], B]: BuilderFactory[(A, B), TreeMap[A, B], Coll] = new BuilderFactory[(A, B), TreeMap[A, B], Coll] { def apply(from: Coll) = newBuilder[A, B] }
  def newBuilder[A <% Ordered[A], B]: Builder[(A, B), TreeMap[A, B], Any] = new ImmutableMapBuilder(empty[A, B])

  /** The empty map of this type */
  def empty[A <% Ordered[A], B] = new TreeMap[A, B]

  /** The canonical factory for this type
   */
  def apply[A <% Ordered[A], B](elems: (A, B)*) = empty[A, B] ++ elems
}

/** This class implements immutable maps using a tree.
 *
 *  @author  Erik Stenman
 *  @author  Matthias Zenger
 *  @version 1.1, 03/05/2004
 */
@serializable
class TreeMap[A <% Ordered[A], +B](override val size: Int, t: RedBlack[A]#Tree[B])
  extends RedBlack[A]
     with SortedMap[A, B]
     with SortedMapTemplate[A, B, TreeMap[A, B]]
     with ImmutableMapTemplate[A, B, TreeMap[A, B]] {

  override protected[this] def newBuilder : Builder[(A, B), TreeMap[A, B], Any] =
    TreeMap.newBuilder[A, B]

  def isSmaller(x: A, y: A) = x < y

  def this() = this(0, null)

  protected val tree: RedBlack[A]#Tree[B] = if (size == 0) Empty else t

  override def rangeImpl(from : Option[A], until : Option[A]): TreeMap[A,B] = {
    val ntree = tree.range(from,until)
    new TreeMap[A,B](ntree.count, ntree)
  }

  override def firstKey = t.first
  override def lastKey = t.last
  override def compare(k0: A, k1: A): Int = k0.compare(k1)

  private def newMap[B](s: Int, t: RedBlack[A]#Tree[B]) = new TreeMap[A, B](s, t)

  /** A factory to create empty maps of the same type of keys.
   */
  override def empty = TreeMap.empty

  /** A new TreeMap with the entry added is returned,
   *  if key is <em>not</em> in the TreeMap, otherwise
   *  the key is updated with the new entry.
   *
   *  @param key ...
   *  @param value ...
   *  @return ...
   */
  def add [B1 >: B](key: A, value: B1): TreeMap[A, B1] = {
    val newsize = if (tree.lookup(key).isEmpty) size + 1 else size
    newMap(newsize, tree.update(key, value))
  }

    /** Add a key/value pair to this map.
   *  @param    kv the key/value pair
   *  @return   A new map with the new binding added to this map
   */
  override def + [B1 >: B] (kv: (A, B1)): TreeMap[A, B1] = add(kv._1, kv._2)

  /** Adds two or more elements to this collection and returns
   *  either the collection itself (if it is mutable), or a new collection
   *  with the added elements.
   *
   *  @param elem1 the first element to add.
   *  @param elem2 the second element to add.
   *  @param elems the remaining elements to add.
   */
  override def + [B1 >: B] (elem1: (A, B1), elem2: (A, B1), elems: (A, B1) *): TreeMap[A, B1] =
    this + elem1 + elem2 ++ collection.Iterable.fromOld(elems)

  /** A new TreeMap with the entry added is returned,
   *  assuming that key is <em>not</em> in the TreeMap.
   */
  def insert [B1 >: B](key: A, value: B1): TreeMap[A, B1] = {
    assert(tree.lookup(key).isEmpty)
    newMap(size + 1, tree.update(key, value))
  }

  def - (key:A): TreeMap[A, B] =
    if (tree.lookup(key).isEmpty) this
    else newMap(size - 1, tree.delete(key))

  /** Check if this map maps <code>key</code> to a value and return the
   *  value if it exists.
   *
   *  @param  key     the key of the mapping of interest
   *  @return the value of the mapping, if it exists
   */
  override def get(key: A): Option[B] = tree.lookup(key) match {
    case n: NonEmpty[b] => Some(n.value)
    case _ => None
  }

  /** Creates a new iterator over all elements contained in this
   *  object.
   *
   *  @return the new iterator
   */
  def elements: Iterator[(A, B)] = tree.toStream.elements

  override def toStream: Stream[(A, B)] = tree.toStream

  override def foreach(f : ((A,B)) => Unit) = tree foreach { case (x, y) => f(x, y) }
}




