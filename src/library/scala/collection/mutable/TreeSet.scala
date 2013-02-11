/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection
package mutable

import generic._

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
  override def clone(): TreeSet[A] = {
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

  // TODO see the discussion on keysIteratorFrom
  override def iterator: Iterator[A] = resolve.avl.iterator
    .dropWhile(e => !isLeftAcceptable(from, ordering)(e))
      .takeWhile(e => isRightAcceptable(until, ordering)(e))
  
  // TODO because TreeSets are potentially ranged views into other TreeSets
  // what this really needs to do is walk the whole stack of tree sets, find
  // the highest "from", and then do a tree walk of the underlying avl tree
  // to find that spot in max(O(stack depth), O(log tree.size)) time which
  // should effectively be O(log size) since ranged views are rare and
  // even more rarely deep. With the following implementation it's
  // O(N log N) to get an iterator from a start point.  
  // But before engaging that endeavor I think mutable.TreeSet should be
  // based on the same immutable RedBlackTree that immutable.TreeSet is
  // based on. There's no good reason to have these two collections based
  // on two different balanced binary trees. That'll save
  // having to duplicate logic for finding the starting point of a
  // sorted binary tree iterator, logic that has already been
  // baked into RedBlackTree.
  override def keysIteratorFrom(start: A) = from(start).iterator

}
