/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection
package mutable

import scala.collection.Stepper.EfficientSplit
import scala.collection.generic.DefaultSerializable
import scala.collection.mutable.{RedBlackTree => RB}

/**
  * A mutable sorted map implemented using a mutable red-black tree as underlying data structure.
  *
  * @param ordering the implicit ordering used to compare objects of type `A`.
  * @tparam K the type of the keys contained in this tree map.
  * @tparam V the type of the values associated with the keys.
  *
  * @define Coll mutable.TreeMap
  * @define coll mutable tree map
  */
sealed class TreeMap[K, V] private (tree: RB.Tree[K, V])(implicit val ordering: Ordering[K])
  extends AbstractMap[K, V]
    with SortedMap[K, V]
    with SortedMapOps[K, V, TreeMap, TreeMap[K, V]]
    with StrictOptimizedIterableOps[(K, V), Iterable, TreeMap[K, V]]
    with StrictOptimizedMapOps[K, V, Map, TreeMap[K, V]]
    with StrictOptimizedSortedMapOps[K, V, TreeMap, TreeMap[K, V]]
    with SortedMapFactoryDefaults[K, V, TreeMap, Iterable, Map]
    with DefaultSerializable {

  override def sortedMapFactory: TreeMap.type = TreeMap

  /**
    * Creates an empty `TreeMap`.
    * @param ord the implicit ordering used to compare objects of type `K`.
    * @return an empty `TreeMap`.
    */
  def this()(implicit ord: Ordering[K]) = this(RB.Tree.empty)(ord)

  def iterator: Iterator[(K, V)] = {
    if (isEmpty) Iterator.empty
    else RB.iterator(tree)
  }

  override def keysIterator: Iterator[K] = {
    if (isEmpty) Iterator.empty
    else RB.keysIterator(tree, None)
  }

  override def valuesIterator: Iterator[V] = {
    if (isEmpty) Iterator.empty
    else RB.valuesIterator(tree, None)
  }

  def keysIteratorFrom(start: K): Iterator[K] = {
    if (isEmpty) Iterator.empty
    else RB.keysIterator(tree, Some(start))
  }

  def iteratorFrom(start: K): Iterator[(K, V)] = {
    if (isEmpty) Iterator.empty
    else RB.iterator(tree, Some(start))
  }

  override def valuesIteratorFrom(start: K): Iterator[V] = {
    if (isEmpty) Iterator.empty
    else RB.valuesIterator(tree, Some(start))
  }

  override def stepper[S <: Stepper[_]](implicit shape: StepperShape[(K, V), S]): S with EfficientSplit =
    shape.parUnbox(
      scala.collection.convert.impl.AnyBinaryTreeStepper.from[(K, V), RB.Node[K, V]](
        size, tree.root, _.left, _.right, x => (x.key, x.value)
      )
    )

  override def keyStepper[S <: Stepper[_]](implicit shape: StepperShape[K, S]): S with EfficientSplit = {
    import scala.collection.convert.impl._
    type T = RB.Node[K, V]
    val s = shape.shape match {
      case StepperShape.IntShape    => IntBinaryTreeStepper.from[T]   (size, tree.root, _.left, _.right, _.key.asInstanceOf[Int])
      case StepperShape.LongShape   => LongBinaryTreeStepper.from[T]  (size, tree.root, _.left, _.right, _.key.asInstanceOf[Long])
      case StepperShape.DoubleShape => DoubleBinaryTreeStepper.from[T](size, tree.root, _.left, _.right, _.key.asInstanceOf[Double])
      case _         => shape.parUnbox(AnyBinaryTreeStepper.from[K, T](size, tree.root, _.left, _.right, _.key))
    }
    s.asInstanceOf[S with EfficientSplit]
  }

  override def valueStepper[S <: Stepper[_]](implicit shape: StepperShape[V, S]): S with EfficientSplit = {
    import scala.collection.convert.impl._
    type T = RB.Node[K, V]
    val s = shape.shape match {
      case StepperShape.IntShape    => IntBinaryTreeStepper.from[T]    (size, tree.root, _.left, _.right, _.value.asInstanceOf[Int])
      case StepperShape.LongShape   => LongBinaryTreeStepper.from[T]   (size, tree.root, _.left, _.right, _.value.asInstanceOf[Long])
      case StepperShape.DoubleShape => DoubleBinaryTreeStepper.from[T] (size, tree.root, _.left, _.right, _.value.asInstanceOf[Double])
      case _         => shape.parUnbox(AnyBinaryTreeStepper.from[V, T] (size, tree.root, _.left, _.right, _.value))
    }
    s.asInstanceOf[S with EfficientSplit]
  }

  def addOne(elem: (K, V)): this.type = { RB.insert(tree, elem._1, elem._2); this }

  def subtractOne(elem: K): this.type = { RB.delete(tree, elem); this }

  override def clear(): Unit = RB.clear(tree)

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
  override def foreachEntry[U](f: (K, V) => U): Unit = RB.foreachEntry(tree, f)

  override def size: Int = RB.size(tree)
  override def knownSize: Int = size
  override def isEmpty: Boolean = RB.isEmpty(tree)

  override def contains(key: K): Boolean = RB.contains(tree, key)

  override def head: (K, V) = RB.min(tree).get

  override def last: (K, V) = RB.max(tree).get

  override def minAfter(key: K): Option[(K, V)] = RB.minAfter(tree, key)

  override def maxBefore(key: K): Option[(K, V)] = RB.maxBefore(tree, key)

  override protected[this] def className: String = "TreeMap"


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

    override def iterator = if (RB.size(tree) == 0) Iterator.empty else RB.iterator(tree, from, until)
    override def keysIterator: Iterator[K] = if (RB.size(tree) == 0) Iterator.empty else RB.keysIterator(tree, from, until)
    override def valuesIterator: Iterator[V] = if (RB.size(tree) == 0) Iterator.empty else RB.valuesIterator(tree, from, until)
    override def keysIteratorFrom(start: K) = if (RB.size(tree) == 0) Iterator.empty else RB.keysIterator(tree, pickLowerBound(Some(start)), until)
    override def iteratorFrom(start: K) = if (RB.size(tree) == 0) Iterator.empty else RB.iterator(tree, pickLowerBound(Some(start)), until)
    override def valuesIteratorFrom(start: K) = if (RB.size(tree) == 0) Iterator.empty else RB.valuesIterator(tree, pickLowerBound(Some(start)), until)
    override def size = if (RB.size(tree) == 0) 0 else iterator.length
    override def knownSize: Int = if (RB.size(tree) == 0) 0 else -1
    override def isEmpty = RB.size(tree) == 0 || !iterator.hasNext
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
    override def foreach[U](f: ((K, V)) => U): Unit = iterator.foreach(f)

    override def clone() = super.clone().rangeImpl(from, until)
  }

}

/**
  * $factoryInfo
  *
  * @define Coll mutable.TreeMap
  * @define coll mutable tree map
  */
@SerialVersionUID(3L)
object TreeMap extends SortedMapFactory[TreeMap] {

  def from[K : Ordering, V](it: IterableOnce[(K, V)]): TreeMap[K, V] =
    Growable.from(empty[K, V], it)

  def empty[K : Ordering, V]: TreeMap[K, V] = new TreeMap[K, V]()

  def newBuilder[K: Ordering, V]: Builder[(K, V), TreeMap[K, V]] = new GrowableBuilder(empty[K, V])

}
