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
import scala.collection.mutable.{RedBlackTree => RB}

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

  /** $sortedMapCanBuildFromInfo */
  implicit def canBuildFrom[A](implicit ord: Ordering[A]): CanBuildFrom[Coll, A, TreeSet[A]] =
    new SortedSetCanBuildFrom[A]
}

/**
 * A mutable sorted set implemented using a mutable red-black tree as underlying data structure.
 *
 * @param ordering the implicit ordering used to compare objects of type `A`.
 * @tparam A the type of the keys contained in this tree set.
 *
 * @author Rui Gonçalves
 * @version 2.12
 * @since 2.10
 *
 * @define Coll mutable.TreeSet
 * @define coll mutable tree set
 */
// Original API designed in part by Lucien Pereira
@SerialVersionUID(-3642111301929493640L)
sealed class TreeSet[A] private (tree: RB.Tree[A, Null])(implicit val ordering: Ordering[A])
  extends AbstractSortedSet[A]
  with SortedSet[A]
  with SetLike[A, TreeSet[A]]
  with SortedSetLike[A, TreeSet[A]]
  with Serializable {

  if (ordering eq null)
    throw new NullPointerException("ordering must not be null")

  /**
   * Creates an empty `TreeSet`.
   * @param ord the implicit ordering used to compare objects of type `A`.
   * @return an empty `TreeSet`.
   */
  def this()(implicit ord: Ordering[A]) = this(RB.Tree.empty)(ord)

  override def empty = TreeSet.empty
  override protected[this] def newBuilder = TreeSet.newBuilder[A]

  /**
   * Creates a ranged projection of this set. Any mutations in the ranged projection affect will update the original set
   * and vice versa.
   *
   * Only keys between this projection's key range will ever appear as elements of this set, independently of whether
   * the elements are added through the original set or through this view. That means that if one inserts an element in
   * a view whose key is outside the view's bounds, calls to `contains` will _not_ consider the newly added element.
   * Mutations are always reflected in the original set, though.
   *
   * @param from the lower bound (inclusive) of this projection wrapped in a `Some`, or `None` if there is no lower
   *             bound.
   * @param until the upper bound (exclusive) of this projection wrapped in a `Some`, or `None` if there is no upper
   *              bound.
   */
  def rangeImpl(from: Option[A], until: Option[A]): TreeSet[A] = new TreeSetView(from, until)

  def -=(key: A): this.type = { RB.delete(tree, key); this }
  def +=(elem: A): this.type = { RB.insert(tree, elem, null); this }

  def contains(elem: A) = RB.contains(tree, elem)

  def iterator = RB.keysIterator(tree)
  def keysIteratorFrom(start: A) = RB.keysIterator(tree, Some(start))
  override def iteratorFrom(start: A) = RB.keysIterator(tree, Some(start))

  override def size = RB.size(tree)
  override def isEmpty = RB.isEmpty(tree)

  override def head = RB.minKey(tree).get
  override def headOption = RB.minKey(tree)
  override def last = RB.maxKey(tree).get
  override def lastOption = RB.maxKey(tree)

  override def foreach[U](f: A => U): Unit = RB.foreachKey(tree, f)
  override def clear(): Unit = RB.clear(tree)

  override def stringPrefix = "TreeSet"

  /**
   * A ranged projection of a [[TreeSet]]. Mutations on this set affect the original set and vice versa.
   *
   * Only keys between this projection's key range will ever appear as elements of this set, independently of whether
   * the elements are added through the original set or through this view. That means that if one inserts an element in
   * a view whose key is outside the view's bounds, calls to `contains` will _not_ consider the newly added element.
   * Mutations are always reflected in the original set, though.
   *
   * @param from the lower bound (inclusive) of this projection wrapped in a `Some`, or `None` if there is no lower
   *             bound.
   * @param until the upper bound (exclusive) of this projection wrapped in a `Some`, or `None` if there is no upper
   *              bound.
   */
  @SerialVersionUID(7087824939194006086L)
  private[this] final class TreeSetView(from: Option[A], until: Option[A]) extends TreeSet[A](tree) {

    /**
     * Given a possible new lower bound, chooses and returns the most constraining one (the maximum).
     */
    private[this] def pickLowerBound(newFrom: Option[A]): Option[A] = (from, newFrom) match {
      case (Some(fr), Some(newFr)) => Some(ordering.max(fr, newFr))
      case (None, _) => newFrom
      case _ => from
    }

    /**
     * Given a possible new upper bound, chooses and returns the most constraining one (the minimum).
     */
    private[this] def pickUpperBound(newUntil: Option[A]): Option[A] = (until, newUntil) match {
      case (Some(unt), Some(newUnt)) => Some(ordering.min(unt, newUnt))
      case (None, _) => newUntil
      case _ => until
    }

    /**
     * Returns true if the argument is inside the view bounds (between `from` and `until`).
     */
    private[this] def isInsideViewBounds(key: A): Boolean = {
      val afterFrom = from.isEmpty || ordering.compare(from.get, key) <= 0
      val beforeUntil = until.isEmpty || ordering.compare(key, until.get) < 0
      afterFrom && beforeUntil
    }

    override def rangeImpl(from: Option[A], until: Option[A]): TreeSet[A] =
      new TreeSetView(pickLowerBound(from), pickUpperBound(until))

    override def contains(key: A) = isInsideViewBounds(key) && RB.contains(tree, key)

    override def iterator = RB.keysIterator(tree, from, until)
    override def keysIteratorFrom(start: A) = RB.keysIterator(tree, pickLowerBound(Some(start)), until)
    override def iteratorFrom(start: A) = RB.keysIterator(tree, pickLowerBound(Some(start)), until)

    override def size = iterator.length
    override def isEmpty = !iterator.hasNext

    override def head = headOption.get
    override def headOption = {
      val elem = if (from.isDefined) RB.minKeyAfter(tree, from.get) else RB.minKey(tree)
      (elem, until) match {
        case (Some(e), Some(unt)) if ordering.compare(e, unt) >= 0 => None
        case _ => elem
      }
    }

    override def last = lastOption.get
    override def lastOption = {
      val elem = if (until.isDefined) RB.maxKeyBefore(tree, until.get) else RB.maxKey(tree)
      (elem, from) match {
        case (Some(e), Some(fr)) if ordering.compare(e, fr) < 0 => None
        case _ => elem
      }
    }

    // Using the iterator should be efficient enough; if performance is deemed a problem later, a specialized
    // `foreachKey(f, from, until)` method can be created in `RedBlackTree`. See
    // https://github.com/scala/scala/pull/4608#discussion_r34307985 for a discussion about this.
    override def foreach[U](f: A => U): Unit = iterator.foreach(f)

    override def clone() = super.clone().rangeImpl(from, until)
  }
}
