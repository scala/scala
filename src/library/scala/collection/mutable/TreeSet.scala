/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection
package mutable

import generic._
import scala.collection.immutable.{RedBlackTree => RB}
import scala.runtime.ObjectRef

/**
 * @define Coll `mutable.TreeSet`
 * @define coll mutable tree set
 * @factoryInfo
 *   Companion object of TreeSet providing factory related utilities.
 *
 * @author Lucien Pereira
 *
 */
object TreeSet extends MutableSortedSetFactory[TreeSet] {
  /**
   *  The empty set of this type
   */
  def empty[A](implicit ordering: Ordering[A]) = new TreeSet[A]()

}

/**
 * A mutable SortedSet using an immutable RedBlack Tree as underlying data structure.
 *
 * @author Lucien Pereira
 *
 */
@deprecatedInheritance("TreeSet is not designed to enable meaningful subclassing.", "2.11.0")
class TreeSet[A] private (treeRef: ObjectRef[RB.Tree[A, Null]], from: Option[A], until: Option[A])(implicit val ordering: Ordering[A])
  extends SortedSet[A] with SetLike[A, TreeSet[A]]
  with SortedSetLike[A, TreeSet[A]] with Set[A] with Serializable {

  if (ordering eq null)
    throw new NullPointerException("ordering must not be null")

  def this()(implicit ordering: Ordering[A]) = this(new ObjectRef(null), None, None)

  override def size: Int = RB.countInRange(treeRef.elem, from, until)

  override def stringPrefix = "TreeSet"

  override def empty: TreeSet[A] = TreeSet.empty

  private def pickBound(comparison: (A, A) => A, oldBound: Option[A], newBound: Option[A]) = (newBound, oldBound) match {
    case (Some(newB), Some(oldB)) => Some(comparison(newB, oldB))
    case (None, _) => oldBound
    case _ => newBound
  }

  override def rangeImpl(fromArg: Option[A], untilArg: Option[A]): TreeSet[A] = {
    val newFrom = pickBound(ordering.max, fromArg, from)
    val newUntil = pickBound(ordering.min, untilArg, until)

    new TreeSet(treeRef, newFrom, newUntil)
  }

  override def -=(elem: A): this.type = {
    treeRef.elem = RB.delete(treeRef.elem, elem)
    this
  }

  override def +=(elem: A): this.type = {
    treeRef.elem = RB.update(treeRef.elem, elem, null, overwrite = false)
    this
  }

  /**
   * Thanks to the immutable nature of the
   * underlying Tree, we can share it with
   * the clone. So clone complexity in time is O(1).
   *
   */
  override def clone(): TreeSet[A] =
    new TreeSet[A](new ObjectRef(treeRef.elem), from, until)

  private val notProjection = !(from.isDefined || until.isDefined)

  override def contains(elem: A): Boolean = {
    def leftAcceptable: Boolean = from match {
      case Some(lb) => ordering.gteq(elem, lb)
      case _ => true
    }

    def rightAcceptable: Boolean = until match {
      case Some(ub) => ordering.lt(elem, ub)
      case _ => true
    }

    (notProjection || (leftAcceptable && rightAcceptable)) &&
      RB.contains(treeRef.elem, elem)
  }

  override def iterator: Iterator[A] = iteratorFrom(None)

  override def keysIteratorFrom(start: A) = iteratorFrom(Some(start))

  private def iteratorFrom(start: Option[A]) = {
    val it = RB.keysIterator(treeRef.elem, pickBound(ordering.max, from, start))
    until match {
      case None => it
      case Some(ub) => it takeWhile (k => ordering.lt(k, ub))
    }
  }
}
