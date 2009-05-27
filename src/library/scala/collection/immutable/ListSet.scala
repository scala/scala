/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.immutable

import generic._

/** The canonical factory of <a href="ListSet.html">ListSet</a>'s */
object ListSet extends SetFactory[ListSet] {
  implicit def builderFactory[A]: BuilderFactory[A, ListSet[A], Coll] = setBuilderFactory[A]
  override def empty[A] = new ListSet[A]
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
class ListSet[A] extends Set[A]
                    with SetClass[A, ListSet]
                    with SetTemplate[A, ListSet[A]] { self =>
  override def companion: Companion[ListSet] = ListSet

  /** Returns the number of elements in this set.
   *
   *  @return number of set elements.
   */
  override def size: Int = 0
  override def isEmpty: Boolean = true;

  /** Checks if this set contains element <code>elem</code>.
   *
   *  @param  elem    the element to check for membership.
   *  @return true, iff <code>elem</code> is contained in this set.
   */
  def contains(elem: A): Boolean = false

  /** This method creates a new set with an additional element.
   */
  def + (elem: A): ListSet[A] = new Node(elem)

  /** <code>-</code> can be used to remove a single element from
   *  a set.
   */
  def - (elem: A): ListSet[A] = this

  /** Creates a new iterator over all elements contained in this set.
   *
   *  @throws Predef.NoSuchElementException
   *  @return the new iterator
   */
  def iterator: Iterator[A] = new Iterator[A] {
    var that: ListSet[A] = self;
    def hasNext = !that.isEmpty;
    def next: A =
      if (!hasNext) throw new NoSuchElementException("next on empty iterator")
      else { val res = that.elem; that = that.next; res }
  }

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
    override def size = self.size + 1

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
    override def contains(e: A) = (e == elem) || self.contains(e)

    /** This method creates a new set with an additional element.
     */
    override def +(e: A): ListSet[A] = if (contains(e)) this else new Node(e)

    /** <code>-</code> can be used to remove a single element from
     *  a set.
     */
    override def -(e: A): ListSet[A] = if (e == elem) self else {
      val tail = self - e; new tail.Node(elem)
    }

    override protected def next: ListSet[A] = self

    override def stringPrefix = "Set"
  }
}
