/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

// todo: make balanced once Tree.scala is updated to be covariant.

package scala.collection.immutable


object TreeMap {

  /** The empty map of this type
   *  @deprecated   use <code>empty</code> instead
   */
  @deprecated
  def Empty[A <% Ordered[A], B] = empty[A, B]

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
class TreeMap[A <% Ordered[A], +B](val size: int, t: RedBlack[A]#Tree[B])
extends RedBlack[A] with SortedMap[A, B] {

  def isSmaller(x: A, y: A) = x < y

  def this() = this(0, null)

  protected val tree: RedBlack[A]#Tree[B] = if (size == 0) Empty else t

  override def rangeImpl(from : Option[A], until : Option[A]) : SortedMap[A,B] = {
    val ntree = tree.range(from,until)
    new TreeMap[A,B](ntree.count, ntree)
  }
  override def first = t.first
  override def last = t.last
  override def compare(k0: A, k1: A): Int = k0.compare(k1)




  private def newMap[B](s: int, t: RedBlack[A]#Tree[B]) = new TreeMap[A, B](s, t)

  /** A factory to create empty maps of the same type of keys.
   */
  def empty[C] = TreeMap.empty[A, C]

  /** A new TreeMap with the entry added is returned,
   *  if key is <em>not</em> in the TreeMap, otherwise
   *  the key is updated with the new entry.
   *
   *  @param key ...
   *  @param value ...
   *  @return ...
   */
  def update [B1 >: B](key: A, value: B1): TreeMap[A, B1] = {
    val newsize = if (tree.lookup(key).isEmpty) size + 1 else size
    newMap(newsize, tree.update(key, value))
  }

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

  /** Retrieve the value which is associated with the given key. This
   *  method throws an exception if there is no mapping from the given
   *  key to a value.
   *
   *  @param  key     the key
   *  @return the value associated with the given key.
   *  @throws Error("key not found").
   */
  override def apply(key: A): B = tree.lookup(key) match {
    case n: NonEmpty[b] => n.value
    case _ => super.apply(key)
  }

  /** Creates a new iterator over all elements contained in this
   *  object.
   *
   *  @return the new iterator
   */
  def elements: Iterator[Pair[A, B]] = tree.elements.elements

  override def foreach(f : Tuple2[A,B] => Unit) : Unit =
    tree.visit[Unit](())((unit0,a,b) => Tuple2(true, f(Tuple2(a,b))))
  override def forall(f : Tuple2[A,B] => Boolean) : Boolean =
    tree.visit[Boolean](true)((input,a,b) => f(Tuple2(a,b)) match {
    case ret if input => Tuple2(ret,ret)
    })._2
  override def exists(f : Tuple2[A,B] => Boolean) : Boolean =
    tree.visit[Boolean](false)((input,a,b) => f(Tuple2(a,b)) match {
    case ret if !input => Tuple2(!ret,ret)
    })._2

}




