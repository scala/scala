package scala
package collection
package mutable

import scala.collection.generic._
import scala.collection.mutable.{RedBlackTree => RB}

/**
 * $factoryInfo
 *
 * @define Coll mutable.TreeMap
 * @define coll mutable tree map
 */
object TreeMap extends MutableSortedMapFactory[TreeMap] {

  def empty[A, B](implicit ord: Ordering[A]) = new TreeMap[A, B]()(ord)

  /** $sortedMapCanBuildFromInfo */
  implicit def canBuildFrom[A, B](implicit ord: Ordering[A]): CanBuildFrom[Coll, (A, B), TreeMap[A, B]] =
    new SortedMapCanBuildFrom[A, B]
}

/**
 * A mutable sorted map implemented using a mutable red-black tree as underlying data structure.
 *
 * @param ordering the implicit ordering used to compare objects of type `A`.
 * @tparam A the type of the keys contained in this tree map.
 * @tparam B the type of the values associated with the keys.
 *
 * @author Rui Gonçalves
 * @version 2.12
 * @since 2.12
 *
 * @define Coll mutable.TreeMap
 * @define coll mutable tree map
 */
@SerialVersionUID(-2558985573956740112L)
sealed class TreeMap[A, B] private (tree: RB.Tree[A, B])(implicit val ordering: Ordering[A])
  extends AbstractSortedMap[A, B]
  with SortedMap[A, B]
  with MapLike[A, B, TreeMap[A, B]]
  with SortedMapLike[A, B, TreeMap[A, B]]
  with Serializable {

  /**
   * Creates an empty `TreeMap`.
   * @param ord the implicit ordering used to compare objects of type `A`.
   * @return an empty `TreeMap`.
   */
  def this()(implicit ord: Ordering[A]) = this(RB.Tree.empty)(ord)

  override def empty = TreeMap.empty
  override protected[this] def newBuilder = TreeMap.newBuilder[A, B]

  /**
   * Creates a ranged projection of this map. Any mutations in the ranged projection will update the original map and
   * vice versa.
   *
   * Only entries with keys between this projection's key range will ever appear as elements of this map, independently
   * of whether the entries are added through the original map or through this view. That means that if one inserts a
   * key-value in a view whose key is outside the view's bounds, calls to `get` or `contains` will _not_ consider the
   * newly added entry. Mutations are always reflected in the original map, though.
   *
   * @param from the lower bound (inclusive) of this projection wrapped in a `Some`, or `None` if there is no lower
   *             bound.
   * @param until the upper bound (exclusive) of this projection wrapped in a `Some`, or `None` if there is no upper
   *              bound.
   */
  def rangeImpl(from: Option[A], until: Option[A]): TreeMap[A, B] = new TreeMapView(from, until)

  def -=(key: A): this.type = { RB.delete(tree, key); this }
  def +=(kv: (A, B)): this.type = { RB.insert(tree, kv._1, kv._2); this }

  def get(key: A) = RB.get(tree, key)

  def iterator = RB.iterator(tree)
  def iteratorFrom(start: A) = RB.iterator(tree, Some(start))
  def keysIteratorFrom(start: A) = RB.keysIterator(tree, Some(start))
  def valuesIteratorFrom(start: A) = RB.valuesIterator(tree, Some(start))

  override def size = RB.size(tree)
  override def isEmpty = RB.isEmpty(tree)
  override def contains(key: A) = RB.contains(tree, key)

  override def head = RB.min(tree).get
  override def headOption = RB.min(tree)
  override def last = RB.max(tree).get
  override def lastOption = RB.max(tree)

  override def keysIterator = RB.keysIterator(tree)
  override def valuesIterator = RB.valuesIterator(tree)

  override def foreach[U](f: ((A, B)) => U): Unit = RB.foreach(tree, f)
  override def transform(f: (A, B) => B) = { RB.transform(tree, f); this }
  override def clear(): Unit = RB.clear(tree)

  override def stringPrefix = "TreeMap"

  /**
   * A ranged projection of a [[TreeMap]]. Mutations on this map affect the original map and vice versa.
   *
   * Only entries with keys between this projection's key range will ever appear as elements of this map, independently
   * of whether the entries are added through the original map or through this view. That means that if one inserts a
   * key-value in a view whose key is outside the view's bounds, calls to `get` or `contains` will _not_ consider the
   * newly added entry. Mutations are always reflected in the original map, though.
   *
   * @param from the lower bound (inclusive) of this projection wrapped in a `Some`, or `None` if there is no lower
   *             bound.
   * @param until the upper bound (exclusive) of this projection wrapped in a `Some`, or `None` if there is no upper
   *              bound.
   */
  @SerialVersionUID(2219159283273389116L)
  private[this] final class TreeMapView(from: Option[A], until: Option[A]) extends TreeMap[A, B](tree) {

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

    override def rangeImpl(from: Option[A], until: Option[A]): TreeMap[A, B] =
      new TreeMapView(pickLowerBound(from), pickUpperBound(until))

    override def get(key: A) = if (isInsideViewBounds(key)) RB.get(tree, key) else None

    override def iterator = RB.iterator(tree, from, until)
    override def iteratorFrom(start: A) = RB.iterator(tree, pickLowerBound(Some(start)), until)
    override def keysIteratorFrom(start: A) = RB.keysIterator(tree, pickLowerBound(Some(start)), until)
    override def valuesIteratorFrom(start: A) = RB.valuesIterator(tree, pickLowerBound(Some(start)), until)

    override def size = iterator.length
    override def isEmpty = !iterator.hasNext
    override def contains(key: A) = isInsideViewBounds(key) && RB.contains(tree, key)

    override def head = headOption.get
    override def headOption = {
      val entry = if (from.isDefined) RB.minAfter(tree, from.get) else RB.min(tree)
      (entry, until) match {
        case (Some(e), Some(unt)) if ordering.compare(e._1, unt) >= 0 => None
        case _ => entry
      }
    }

    override def last = lastOption.get
    override def lastOption = {
      val entry = if (until.isDefined) RB.maxBefore(tree, until.get) else RB.max(tree)
      (entry, from) match {
        case (Some(e), Some(fr)) if ordering.compare(e._1, fr) < 0 => None
        case _ => entry
      }
    }

    // Using the iterator should be efficient enough; if performance is deemed a problem later, specialized
    // `foreach(f, from, until)` and `transform(f, from, until)` methods can be created in `RedBlackTree`. See
    // https://github.com/scala/scala/pull/4608#discussion_r34307985 for a discussion about this.
    override def foreach[U](f: ((A, B)) => U): Unit = iterator.foreach(f)
    override def transform(f: (A, B) => B) = {
      iterator.foreach { case (key, value) => update(key, f(key, value)) }
      this
    }

    override def valuesIterator: Iterator[B] = RB.valuesIterator(tree, from, until)
    override def keysIterator: Iterator[A] = RB.keysIterator(tree, from, until)

    override def clone() = super.clone().rangeImpl(from, until)
  }
}
