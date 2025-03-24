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
package collection.mutable

import scala.collection.Stepper.EfficientSplit
import scala.collection.generic.DefaultSerializable
import scala.collection.mutable.{RedBlackTree => RB}
import scala.collection.{SortedIterableFactory, SortedSetFactoryDefaults, Stepper, StepperShape, StrictOptimizedIterableOps, StrictOptimizedSortedSetOps, mutable}

/**
  * A mutable sorted set implemented using a mutable red-black tree as underlying data structure.
  *
  * @param ordering the implicit ordering used to compare objects of type `A`.
  * @tparam A the type of the keys contained in this tree set.
  *
  * @define Coll mutable.TreeSet
  * @define coll mutable tree set
  */
// Original API designed in part by Lucien Pereira
sealed class TreeSet[A] private (private val tree: RB.Tree[A, Null])(implicit val ordering: Ordering[A])
  extends AbstractSet[A]
    with SortedSet[A]
    with SortedSetOps[A, TreeSet, TreeSet[A]]
    with StrictOptimizedIterableOps[A, Set, TreeSet[A]]
    with StrictOptimizedSortedSetOps[A, TreeSet, TreeSet[A]]
    with SortedSetFactoryDefaults[A, TreeSet, Set]
    with DefaultSerializable {

  if (ordering eq null)
    throw new NullPointerException("ordering must not be null")

  /**
    * Creates an empty `TreeSet`.
    * @param ord the implicit ordering used to compare objects of type `A`.
    * @return an empty `TreeSet`.
    */
  def this()(implicit ord: Ordering[A]) = this(RB.Tree.empty)(ord)

  override def sortedIterableFactory: SortedIterableFactory[TreeSet] = TreeSet

  def iterator: collection.Iterator[A] = RB.keysIterator(tree)

  def iteratorFrom(start: A): collection.Iterator[A] = RB.keysIterator(tree, Some(start))

  override def stepper[S <: Stepper[_]](implicit shape: StepperShape[A, S]): S with EfficientSplit = {
    import scala.collection.convert.impl._
    type T = RB.Node[A, Null]
    val s = shape.shape match {
      case StepperShape.IntShape    => IntBinaryTreeStepper.from[T]   (size, tree.root, _.left, _.right, _.key.asInstanceOf[Int])
      case StepperShape.LongShape   => LongBinaryTreeStepper.from[T]  (size, tree.root, _.left, _.right, _.key.asInstanceOf[Long])
      case StepperShape.DoubleShape => DoubleBinaryTreeStepper.from[T](size, tree.root, _.left, _.right, _.key.asInstanceOf[Double])
      case _         => shape.parUnbox(AnyBinaryTreeStepper.from[A, T](size, tree.root, _.left, _.right, _.key))
    }
    s.asInstanceOf[S with EfficientSplit]
  }

  def addOne(elem: A): this.type = {
    RB.insert(tree, elem, null)
    this
  }

  def subtractOne(elem: A): this.type = {
    RB.delete(tree, elem)
    this
  }

  def clear(): Unit = RB.clear(tree)

  def contains(elem: A): Boolean = RB.contains(tree, elem)

  def unconstrained: collection.Set[A] = this

  def rangeImpl(from: Option[A], until: Option[A]): TreeSet[A] = new TreeSetProjection(from, until)

  override protected[this] def className: String = "TreeSet"

  override def size: Int = RB.size(tree)
  override def knownSize: Int = size
  override def isEmpty: Boolean = RB.isEmpty(tree)

  override def head: A = RB.minKey(tree).get

  override def last: A = RB.maxKey(tree).get

  override def minAfter(key: A): Option[A] = RB.minKeyAfter(tree, key)

  override def maxBefore(key: A): Option[A] = RB.maxKeyBefore(tree, key)

  override def foreach[U](f: A => U): Unit = RB.foreachKey(tree, f)


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
  private[this] final class TreeSetProjection(from: Option[A], until: Option[A]) extends TreeSet[A](tree) {

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
      new TreeSetProjection(pickLowerBound(from), pickUpperBound(until))

    override def contains(key: A) = isInsideViewBounds(key) && RB.contains(tree, key)

    override def iterator = RB.keysIterator(tree, from, until)
    override def iteratorFrom(start: A) = RB.keysIterator(tree, pickLowerBound(Some(start)), until)

    override def size = if (RB.size(tree) == 0) 0 else iterator.length
    override def knownSize: Int = if (RB.size(tree) == 0) 0 else -1
    override def isEmpty: Boolean = RB.size(tree) == 0 || !iterator.hasNext

    override def head: A = headOption.get
    override def headOption: Option[A] = {
      val elem = if (from.isDefined) RB.minKeyAfter(tree, from.get) else RB.minKey(tree)
      (elem, until) match {
        case (Some(e), Some(unt)) if ordering.compare(e, unt) >= 0 => None
        case _ => elem
      }
    }

    override def last: A = lastOption.get
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

    override def clone(): mutable.TreeSet[A] = super.clone().rangeImpl(from, until)

  }

}

/**
  * $factoryInfo
  * @define Coll `mutable.TreeSet`
  * @define coll mutable tree set
  */
@SerialVersionUID(3L)
object TreeSet extends SortedIterableFactory[TreeSet] {

  def empty[A : Ordering]: TreeSet[A] = new TreeSet[A]()

  def from[E](it: IterableOnce[E])(implicit ordering: Ordering[E]): TreeSet[E] =
    it match {
      case ts: TreeSet[E] if ordering == ts.ordering =>
        new TreeSet[E](ts.tree.treeCopy())
      case ss: scala.collection.SortedSet[E] if ordering == ss.ordering =>
        new TreeSet[E](RB.fromOrderedKeys(ss.iterator, ss.size))
      case r: Range if (ordering eq Ordering.Int) || (ordering eq Ordering.Int.reverse) =>
        val it = if((ordering eq Ordering.Int) == (r.step > 0)) r.iterator else r.reverseIterator
        new TreeSet[E](RB.fromOrderedKeys(it.asInstanceOf[Iterator[E]], r.size))
      case _ =>
        val t: RB.Tree[E, Null] = RB.Tree.empty
        val i = it.iterator
        while (i.hasNext) RB.insert(t, i.next(), null)
        new TreeSet[E](t)
    }

  def newBuilder[A](implicit ordering: Ordering[A]): Builder[A, TreeSet[A]] = new ReusableBuilder[A, TreeSet[A]] {
    private[this] var tree: RB.Tree[A, Null] = RB.Tree.empty
    def addOne(elem: A): this.type = { RB.insert(tree, elem, null); this }
    def result(): TreeSet[A] = new TreeSet[A](tree)
    def clear(): Unit = { tree = RB.Tree.empty }
  }
}
