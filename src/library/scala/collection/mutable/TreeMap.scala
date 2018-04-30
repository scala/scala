package scala
package collection.mutable

import collection.{Iterator, SortedMapFactory, StrictOptimizedIterableOps}
import collection.mutable.{RedBlackTree => RB}

import java.lang.String

/**
  * A mutable sorted map implemented using a mutable red-black tree as underlying data structure.
  *
  * @param ordering the implicit ordering used to compare objects of type `A`.
  * @tparam K the type of the keys contained in this tree map.
  * @tparam V the type of the values associated with the keys.
  *
  * @author Rui Gonçalves
  * @version 2.12
  * @since 2.12
  *
  * @define Coll mutable.TreeMap
  * @define coll mutable tree map
  */
@SerialVersionUID(3L)
sealed class TreeMap[K, V] private (tree: RB.Tree[K, V])(implicit val ordering: Ordering[K])
  extends AbstractMap[K, V]
    with SortedMap[K, V]
    with SortedMapOps[K, V, TreeMap, TreeMap[K, V]]
    with StrictOptimizedIterableOps[(K, V), Iterable, TreeMap[K, V]]
    with Serializable {

  override def sortedMapFactory = TreeMap

  /**
    * Creates an empty `TreeMap`.
    * @param ord the implicit ordering used to compare objects of type `K`.
    * @return an empty `TreeMap`.
    */
  def this()(implicit ord: Ordering[K]) = this(RB.Tree.empty)(ord)

  def iterator(): Iterator[(K, V)] = RB.iterator(tree)

  def keysIteratorFrom(start: K): Iterator[K] = RB.keysIterator(tree, Some(start))

  def iteratorFrom(start: K): Iterator[(K, V)] = RB.iterator(tree, Some(start))

  override def valuesIteratorFrom(start: K): Iterator[V] = RB.valuesIterator(tree, Some(start))

  def addOne(elem: (K, V)): this.type = { RB.insert(tree, elem._1, elem._2); this }

  def subtractOne(elem: K): this.type = { RB.delete(tree, elem); this }

  def clear(): Unit = RB.clear(tree)

  def get(key: K): Option[V] = RB.get(tree, key)

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
  def rangeImpl(from: Option[K], until: Option[K]): TreeMap[K, V] = new TreeMapProjection(from, until)

  override def foreach[U](f: ((K, V)) => U): Unit = RB.foreach(tree, f)

  override def size: Int = RB.size(tree)

  override def isEmpty: Boolean = RB.isEmpty(tree)

  override def contains(key: K): Boolean = RB.contains(tree, key)

  override def head: (K, V) = RB.min(tree).get

  override def last: (K, V) = RB.max(tree).get

  override def minAfter(key: K): Option[(K, V)] = RB.minAfter(tree, key)

  override def maxBefore(key: K): Option[(K, V)] = RB.maxBefore(tree, key)

  override def className: String = "TreeMap"


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
  @SerialVersionUID(3L)
  private[this] final class TreeMapProjection(from: Option[K], until: Option[K]) extends TreeMap[K, V](tree) {

    /**
      * Given a possible new lower bound, chooses and returns the most constraining one (the maximum).
      */
    private[this] def pickLowerBound(newFrom: Option[K]): Option[K] = (from, newFrom) match {
      case (Some(fr), Some(newFr)) => Some(ordering.max(fr, newFr))
      case (None, _) => newFrom
      case _ => from
    }

    /**
      * Given a possible new upper bound, chooses and returns the most constraining one (the minimum).
      */
    private[this] def pickUpperBound(newUntil: Option[K]): Option[K] = (until, newUntil) match {
      case (Some(unt), Some(newUnt)) => Some(ordering.min(unt, newUnt))
      case (None, _) => newUntil
      case _ => until
    }

    /**
      * Returns true if the argument is inside the view bounds (between `from` and `until`).
      */
    private[this] def isInsideViewBounds(key: K): Boolean = {
      val afterFrom = from.isEmpty || ordering.compare(from.get, key) <= 0
      val beforeUntil = until.isEmpty || ordering.compare(key, until.get) < 0
      afterFrom && beforeUntil
    }

    override def rangeImpl(from: Option[K], until: Option[K]): TreeMap[K, V] =
      new TreeMapProjection(pickLowerBound(from), pickUpperBound(until))

    override def get(key: K) = if (isInsideViewBounds(key)) RB.get(tree, key) else None

    override def iterator() = RB.iterator(tree, from, until)
    override def keysIteratorFrom(start: K) = RB.keysIterator(tree, pickLowerBound(Some(start)), until)
    override def iteratorFrom(start: K) = RB.iterator(tree, pickLowerBound(Some(start)), until)
    override def valuesIteratorFrom(start: K) = RB.valuesIterator(tree, pickLowerBound(Some(start)), until)

    override def size = iterator().length
    override def isEmpty = !iterator().hasNext
    override def contains(key: K) = isInsideViewBounds(key) && RB.contains(tree, key)

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
    override def foreach[U](f: ((K, V)) => U): Unit = iterator().foreach(f)

    override def clone() = super.clone().rangeImpl(from, until)
  }

}

/**
  * $factoryInfo
  *
  * @define Coll mutable.TreeMap
  * @define coll mutable tree map
  */
object TreeMap extends SortedMapFactory[TreeMap] {

  def from[K : Ordering, V](it: collection.IterableOnce[(K, V)]): TreeMap[K, V] =
    Growable.from(empty[K, V], it)

  def empty[K : Ordering, V]: TreeMap[K, V] = new TreeMap[K, V]()

  def newBuilder[K: Ordering, V](): Builder[(K, V), TreeMap[K, V]] = new GrowableBuilder(empty[K, V])

}
