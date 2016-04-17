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
import scala.annotation.tailrec

/** $factoryInfo
 *  @define Coll immutable.ListSet
 *  @define coll immutable list set
 *  @since 1
 */
object ListSet extends ImmutableSetFactory[ListSet] {
  /** setCanBuildFromInfo */
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, ListSet[A]] = setCanBuildFrom[A]

  private object EmptyListSet extends ListSet[Any] { }
  private[collection] def emptyInstance: ListSet[Any] = EmptyListSet
}

/** This class implements immutable sets using a list-based data
 *  structure. Instances of `ListSet` represent
 *  empty sets; they can be either created by calling the constructor
 *  directly, or by applying the function `ListSet.empty`.
 *
 *  @tparam A    the type of the elements contained in this list set.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 09/07/2003
 *  @since   1
 *  @define Coll immutable.ListSet
 *  @define coll immutable list set
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
sealed class ListSet[A] extends AbstractSet[A]
                    with Set[A]
                    with GenericSetTemplate[A, ListSet]
                    with SetLike[A, ListSet[A]]
                    with Serializable{ self =>
  override def companion: GenericCompanion[ListSet] = ListSet

  /** Returns the number of elements in this set.
   *
   *  @return number of set elements.
   */
  override def size: Int = 0
  override def isEmpty: Boolean = true

  /** Checks if this set contains element `elem`.
   *
   *  @param  elem    the element to check for membership.
   *  @return `'''true'''`, iff `elem` is contained in this set.
   */
  def contains(elem: A): Boolean = false

  /** This method creates a new set with an additional element.
   */
  def + (elem: A): ListSet[A] = new Node(elem)

  /** `-` can be used to remove a single element.
   */
  def - (elem: A): ListSet[A] = this

  /** If we are bulk adding elements and desire a runtime measured in
   *  sub-interstellar time units, we better find a way to avoid traversing
   *  the collection on each element.  That's what the custom builder does,
   *  so we take the easy way out and add ourselves and the argument to
   *  a new builder.
   */
  override def ++(xs: GenTraversableOnce[A]): ListSet[A] =
    if (xs.isEmpty) this
    else (repr /: xs) (_ + _)

  private[ListSet] def unchecked_outer: ListSet[A] =
    throw new NoSuchElementException("Empty ListSet has no outer pointer")

  /** Creates a new iterator over all elements contained in this set.
   *
   *  @throws java.util.NoSuchElementException
   *  @return the new iterator
   */
  def iterator: Iterator[A] = {
    def reverseList = {
      var curr: ListSet[A] = self
      var res: List[A] = Nil
      while (!curr.isEmpty) {
        res = curr.elem :: res
        curr = curr.next
      }
      res
    }
    reverseList.iterator
  }

  /**
   *  @throws java.util.NoSuchElementException
   */
  protected def elem: A = throw new NoSuchElementException("elem of empty set")

  /**
   *  @throws java.util.NoSuchElementException
   */
  protected def next: ListSet[A] = throw new NoSuchElementException("Next of an empty set")

  override def stringPrefix = "ListSet"

  /** Represents an entry in the `ListSet`.
   */
  protected class Node(override val elem: A) extends ListSet[A] with Serializable {
    override private[ListSet] def unchecked_outer = self

    /** Returns the number of elements in this set.
     *
     *  @return number of set elements.
     */
    override def size = sizeInternal(this, 0)
    @tailrec private def sizeInternal(n: ListSet[A], acc: Int): Int =
      if (n.isEmpty) acc
      else sizeInternal(n.unchecked_outer, acc + 1)

    /** Checks if this set is empty.
     *
     *  @return true, iff there is no element in the set.
     */
    override def isEmpty: Boolean = false

    /** Checks if this set contains element `elem`.
     *
     *  @param  e       the element to check for membership.
     *  @return `'''true'''`, iff `elem` is contained in this set.
     */
    override def contains(e: A) = containsInternal(this, e)
    @tailrec private def containsInternal(n: ListSet[A], e: A): Boolean =
      !n.isEmpty && (n.elem == e || containsInternal(n.unchecked_outer, e))

    /** This method creates a new set with an additional element.
     */
    override def +(e: A): ListSet[A] = if (contains(e)) this else new Node(e)

    /** `-` can be used to remove a single element from a set.
     */
    override def -(e: A): ListSet[A] = removeInternal(e, this, Nil)

    @tailrec private def removeInternal(k: A, cur: ListSet[A], acc: List[ListSet[A]]): ListSet[A] =
      if (cur.isEmpty) acc.last
      else if (k == cur.elem) (cur.next /: acc) { case (t, h) => new t.Node(h.elem) }
      else removeInternal(k, cur.next, cur :: acc)

    override protected def next: ListSet[A] = self
  }

  override def toSet[B >: A]: Set[B] = this.asInstanceOf[ListSet[B]]
}
