/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection
package mutable

import generic._

/** 
 * @define Coll mutable.TreeSet
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
 * A mutable SortedSet using an immutable AVL Tree as underlying data structure.
 *
 * @author Lucien Pereira
 * 
 */
class TreeSet[A](implicit val ordering: Ordering[A]) extends SortedSet[A] with SetLike[A, TreeSet[A]]
  with SortedSetLike[A, TreeSet[A]] with Set[A] with Serializable {

  // Projection constructor
  private def this(base: Option[TreeSet[A]], from: Option[A], until: Option[A])(implicit ordering: Ordering[A]) {
    this();
    this.base = base
    this.from = from
    this.until = until
  }

  private var base: Option[TreeSet[A]] = None

  private var from: Option[A] = None

  private var until: Option[A] = None

  private var avl: AVLTree[A] = Leaf

  private var cardinality: Int = 0

  def resolve: TreeSet[A] = base.getOrElse(this)

  private def isLeftAcceptable(from: Option[A], ordering: Ordering[A])(a: A): Boolean =
    from.map(x => ordering.gteq(a, x)).getOrElse(true)

  private def isRightAcceptable(until: Option[A], ordering: Ordering[A])(a: A): Boolean =
    until.map(x => ordering.lt(a, x)).getOrElse(true)

  /**
   * Cardinality store the set size, unfortunately a
   * set view (given by rangeImpl)
   * cannot take advantage of this optimisation
   * 
   */
  override def size: Int = base.map(_ => super.size).getOrElse(cardinality)

  override def stringPrefix = "TreeSet"

  override def empty: TreeSet[A] = TreeSet.empty

  override def rangeImpl(from: Option[A], until: Option[A]): TreeSet[A] = new TreeSet(Some(this), from, until)

  override def -=(elem: A): this.type = {
    try {
      resolve.avl = resolve.avl.remove(elem, ordering)
      resolve.cardinality = resolve.cardinality - 1
    } catch {
      case e: NoSuchElementException => ()
    }
    this
  }

  override def +=(elem: A): this.type = {
    try {
      resolve.avl = resolve.avl.insert(elem, ordering)
      resolve.cardinality = resolve.cardinality + 1
    } catch {
      case e: IllegalArgumentException => ()
    }
    this
  }

  /**
   * Thanks to the immutable nature of the
   * underlying AVL Tree, we can share it with
   * the clone. So clone complexity in time is O(1).
   * 
   */
  override def clone: TreeSet[A] = {
    val clone = new TreeSet[A](base, from, until)
    clone.avl = resolve.avl
    clone.cardinality = resolve.cardinality
    clone
  }

  override def contains(elem: A): Boolean = {
    isLeftAcceptable(from, ordering)(elem) &&
    isRightAcceptable(until, ordering)(elem) &&
    resolve.avl.contains(elem, ordering)
  }

  override def iterator: Iterator[A] = resolve.avl.iterator
    .dropWhile(e => !isLeftAcceptable(from, ordering)(e))
      .takeWhile(e => isRightAcceptable(until, ordering)(e))
  
}
