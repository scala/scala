/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.immutable


//import Predef.NoSuchElementException

object ListSet {

  /** constructs an empty ListSet
   *  @deprecated   use <code>empty</code> instead
   */
  @deprecated
  def Empty[A] = new ListSet[A]

  /** The empty set of this type.
   */
  def empty[A] = new ListSet[A]

  /** The canonical factory for this type
   */
  def apply[A, B](elems: A*) = empty[A] ++ elems
}


/** This class implements immutable sets using a list-based data
 *  structure. Instances of <code>ListSet</code> represent
 *  empty sets; they can be either created by calling the constructor
 *  directly, or by applying the function <code>ListSet.empty</code>.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 09/07/2003
 */
@serializable
class ListSet[A] extends AnyRef with Set[A] {

  /** Returns the number of elements in this set.
   *
   *  @return number of set elements.
   */
  def size: Int = 0

  override def isEmpty: Boolean = true;

  def empty[B] = ListSet.empty[B]

  /** Checks if this set contains element <code>elem</code>.
   *
   *  @param  elem    the element to check for membership.
   *  @return true, iff <code>elem</code> is contained in this set.
   */
  def contains(elem: A): Boolean = false

  /** This method creates a new set with an additional element.
   */
  def +(elem: A): ListSet[A] = new Node(elem)

  /** <code>-</code> can be used to remove a single element from
   *  a set.
   */
  def -(elem: A): ListSet[A] = this

  /** Creates a new iterator over all elements contained in this set.
   *
   *  @throws Predef.NoSuchElementException
   *  @return the new iterator
   */
  def elements: Iterator[A] = new Iterator[A] {
    var that: ListSet[A] = ListSet.this;
    def hasNext = !that.isEmpty;
    def next: A =
      if (!hasNext) throw new NoSuchElementException("next on empty iterator")
      else { val res = that.elem; that = that.next; res }
  }

  /** Compares two sets for equality.
   *   Two set are equal iff they contain the same elements.
   */
  override def equals(obj: Any): Boolean =
    if (obj.isInstanceOf[scala.collection.Set[A]]) {
      val that = obj.asInstanceOf[scala.collection.Set[A]]
      if (size != that.size) false else toList.forall(that.contains)
    } else
      false

  /**
   *  @throws Predef.NoSuchElementException
   */
  protected def elem: A = throw new NoSuchElementException("Set has no elements");

  /**
   *  @throws Predef.NoSuchElementException
   */
  protected def next: ListSet[A] = throw new NoSuchElementException("Next of an empty set");

  @serializable
  protected class Node(override protected val elem: A) extends ListSet[A] {
    /** Returns the number of elements in this set.
     *
     *  @return number of set elements.
     */
    override def size = ListSet.this.size + 1

    /** Checks if this set is empty.
     *
     *  @return true, iff there is no element in the set.
     */
    override def isEmpty: Boolean = false

    /** Checks if this set contains element <code>elem</code>.
     *
     *  @param  elem    the element to check for membership.
     *  @return true, iff <code>elem</code> is contained in this set.
     */
    override def contains(e: A) = (e == elem) || ListSet.this.contains(e)

    /** This method creates a new set with an additional element.
     */
    override def +(e: A): ListSet[A] = if (contains(e)) this else new Node(e)

    /** <code>-</code> can be used to remove a single element from
     *  a set.
     */
    override def -(e: A): ListSet[A] = if (e == elem) ListSet.this else {
      val tail = ListSet.this - e; new tail.Node(elem)
    }

    override protected def next: ListSet[A] = ListSet.this;
  }
}
