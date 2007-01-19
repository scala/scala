/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.immutable

object TreeSet {

  /** The empty set of this type
   *  @deprecated   use <code>empty</code> instead
   */
  [deprecated] def Empty[A <% Ordered[A]] = empty[A]

  /** The empty set of this type
   */
  def empty[A <% Ordered[A]] = new TreeSet[A]

  /** The canonical factory for this type
   */
  def apply[A <% Ordered[A]](elems: A*) = empty[A] ++ elems
}

/** This class implements immutable sets using a tree.
 *
 *  @author  Martin Odersky
 *  @version 2.0, 02/01/2007
 */

[serializable]
class TreeSet[A <% Ordered[A]](tree: RedBlack[A]#Tree[Unit])
extends RedBlack[A] with Set[A] {
  def isSmaller(x: A, y: A) = x < y
  def this() = this(Empty)

  def size = tree.size

  private def newSet(t: RedBlack[A]#Tree[Unit]) = new TreeSet[A](t)

  /** @return an empty set of arbitrary element type
   */
  def empty[B]: Set[B] = ListSet.empty[B]

  /** A new TreeSet with the entry added is returned,
   */
  def + (elem: A): TreeSet[A] = {
    newSet(tree.update(elem, ()))
  }

  /** A new TreeSet with the entry added is returned,
   *  assuming that elem is <em>not</em> in the TreeSet.
   */
  def insert (elem: A): TreeSet[A] = {
    assert(tree.lookup(elem).isEmpty)
    newSet(tree.update(elem, ()))
  }

  def - (elem:A): TreeSet[A] = {
    val newtree = tree.delete(elem)
    if(newtree eq tree)
      this
    else
      newSet(newtree)
  }

  /** Checks if this set contains element <code>elem</code>.
   *
   *  @param  elem    the element to check for membership.
   *  @return true, iff <code>elem</code> is contained in this set.
   */
  def contains(elem: A): Boolean = !tree.lookup(elem).isEmpty

  /** Creates a new iterator over all elements contained in this
   *  object.
   *
   *  @return the new iterator
   */
  def elements: Iterator[A] = tree.elements map (._1)
}
