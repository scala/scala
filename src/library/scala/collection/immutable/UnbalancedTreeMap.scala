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


object UnbalancedTreeMap {

  /** The empty map of this type */
  def empty[A <% Ordered[A], B] = new UnbalancedTreeMap[A, B]

  /** The canonical factory for this type
   */
  def apply[A<% Ordered[A], B](elems: (A, B)*) = empty[A, B] ++ elems
}

/** This class implements immutable maps using a tree.
 *
 *  @author  Martin Odersky
 *  @version 1.1, 02/01/2007
 */

@serializable
class UnbalancedTreeMap[A <% Ordered[A], +B] extends Map[A, B] {

  /** A factory to create empty maps of the same type of keys.
   */
  def empty[C] = UnbalancedTreeMap.empty[A, C]

  def size: Int = 0

  override def isEmpty: boolean = true

  protected def add [B1 >: B](key: A, value: B1) = new Node(key, value, this, this)
  protected def findValue (key: A): UnbalancedTreeMap[A, B] = this

  protected def key: A = throw new NoSuchElementException("empty map")
  protected def value: B = throw new NoSuchElementException("empty map")
  protected def smallest: UnbalancedTreeMap[A, B] = throw new NoSuchElementException("empty map")

  /** A new TreeMap with the entry added is returned,
   *  if key is <em>not</em> in the TreeMap, otherwise
   *  the key is updated with the new entry.
   *
   *  @param key ...
   *  @param value ...
   *  @return ...
   */
  def update [B1 >: B](key: A, value: B1) = add(key, value)

  /** A new TreeMap with the entry added is returned,
   *  assuming that key is <em>not</em> in the TreeMap.
   */
  def insert [B1 >: B](key: A, value: B1) = add(key, value)

  def - (key:A): UnbalancedTreeMap[A, B] = this

  /** Check if this map maps <code>key</code> to a value and return the
   *  value if it exists.
   *
   *  @param  key     the key of the mapping of interest
   *  @return the value of the mapping, if it exists
   */
  override def get(key: A): Option[B] = {
    val t = findValue(key)
    if (t.isEmpty) None
    else Some(t.value)
  }

  /** Retrieve the value which is associated with the given key. This
   *  method throws an exception if there is no mapping from the given
   *  key to a value.
   *
   *  @param  key     the key
   *  @return the value associated with the given key.
   *  @throws Error("key not found").
   */
  override def apply(key: A): B = {
    val t = findValue(key)
    if (!t.isEmpty) t.value
    else super.apply(key)
  }

  /** Creates a new iterator over all elements contained in this
   *  object.
   *
   *  @return the new iterator
   */
  def elements: Iterator[(A, B)] = Iterator.empty

  protected class Node[+B](override protected val key: A,
                           override protected val value: B,
                           left: UnbalancedTreeMap[A, B],
                           right: UnbalancedTreeMap[A, B]) extends UnbalancedTreeMap[A, B]
  {
    override def size = left.size + right.size + 1

    override def isEmpty = false

    override protected def add [B1 >: B](k: A, v: B1) =
      if (k < key) new Node[B1](key, value, left.add(k, v), right)
      else if (k > key) new Node[B1](key, value, left, right.add(k, v))
      else new Node[B1](k, v, left, right)

    override protected def findValue (k: A): UnbalancedTreeMap[A, B] =
      if (k < key) left.findValue(k)
      else if (k > key) right.findValue(k)
      else this

    override protected def smallest: UnbalancedTreeMap[A, B] =
      if (left.isEmpty) this else left.smallest

    override def - (k: A): UnbalancedTreeMap[A, B] =
      if (k < key) new Node(key, value, left - k, right)
      else if (k > key) new Node(key, value, left, right - k)
      else combine(left, right)

    private def combine[B](l: UnbalancedTreeMap[A, B], r: UnbalancedTreeMap[A, B]) = {
      if (l.isEmpty) r
      else if (r.isEmpty) l
      else {
        val s = r.smallest
        new Node(s.key, s.value, l, r - s.key)
      }
    }

    override def elements: Iterator[(A, B)] =
      left.elements append Iterator.single((key, value)) append right.elements
  }
}




