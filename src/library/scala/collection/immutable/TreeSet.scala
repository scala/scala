/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection
package immutable

import generic._
import immutable.{RedBlackTree => RB}
import mutable.Builder

/** $factoryInfo
 *  @define Coll `immutable.TreeSet`
 *  @define coll immutable tree set
 */
object TreeSet extends ImmutableSortedSetFactory[TreeSet] {
  implicit def implicitBuilder[A](implicit ordering: Ordering[A]): Builder[A, TreeSet[A]] = newBuilder[A](ordering)
  override def newBuilder[A](implicit ordering: Ordering[A]): Builder[A, TreeSet[A]] =
    new TreeSetBuilder

  /** The empty set of this type
   */
  def empty[A](implicit ordering: Ordering[A]) = new TreeSet[A]
  private class TreeSetBuilder[A](implicit val ordering: Ordering[A]) extends Builder[A, TreeSet[A]] {
    type Tree = RB.Tree[A, Any]
    private [this] var tree:Tree = null
    override def +=(elem: A): TreeSetBuilder.this.type = {
      tree = RB.update(tree, elem, (), overwrite = false)
      this
    }

    override def ++=(xs: TraversableOnce[A]): this.type = {
      xs match {
        case ts: TreeSet[A] if ts.ordering == ordering =>
          tree = RB.union(tree, ts.tree)
        case _ =>
          super.++=(xs)
      }
      this
    }

    override def clear(): Unit = {
      tree = null
    }

    override def result(): TreeSet[A] = new TreeSet(tree)(ordering)
  }
}

/** This class implements immutable sets using a tree.
 *
 *  @tparam A         the type of the elements contained in this tree set
 *  @param ordering   the implicit ordering used to compare objects of type `A`
 *
 *  @author  Martin Odersky
 *  @since   1
 *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-immutable-collection-classes.html#red-black-trees "Scala's Collection Library overview"]]
 *  section on `Red-Black Trees` for more information.
 *
 *  @define Coll `immutable.TreeSet`
 *  @define coll immutable tree set
 *  @define orderDependent
 *  @define orderDependentFold
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
@SerialVersionUID(-5685982407650748405L)
final class TreeSet[A] private[immutable] (private[immutable] val tree: RB.Tree[A, Any])(implicit val ordering: Ordering[A])
  extends SortedSet[A] with SortedSetLike[A, TreeSet[A]] with Serializable {

  if (ordering eq null)
    throw new NullPointerException("ordering must not be null")

  override def stringPrefix = "TreeSet"

  override def size = RB.count(tree)

  override def head = RB.smallest(tree).key
  override def headOption = if (RB.isEmpty(tree)) None else Some(head)
  override def last = RB.greatest(tree).key
  override def lastOption = if (RB.isEmpty(tree)) None else Some(last)

  override def tail = new TreeSet(RB.delete(tree, firstKey))
  override def init = new TreeSet(RB.delete(tree, lastKey))

  override def min[A1 >: A](implicit ord: Ordering[A1]): A = {
    if ((ord eq ordering) && nonEmpty) {
      head
    } else {
      super.min(ord)
    }
  }

  override def max[A1 >: A](implicit ord: Ordering[A1]): A = {
    if ((ord eq ordering) && nonEmpty) {
      last
    } else {
      super.max(ord)
    }
  }

  override def drop(n: Int) = {
    if (n <= 0) this
    else if (n >= size) empty
    else newSetOrSelf(RB.drop(tree, n))
  }

  override def take(n: Int) = {
    if (n <= 0) empty
    else if (n >= size) this
    else newSetOrSelf(RB.take(tree, n))
  }

  override def slice(from: Int, until: Int) = {
    if (until <= from) empty
    else if (from <= 0) take(until)
    else if (until >= size) drop(from)
    else newSetOrSelf(RB.slice(tree, from, until))
  }

  override def dropRight(n: Int) = take(size - math.max(n, 0))
  override def takeRight(n: Int) = drop(size - math.max(n, 0))
  override def splitAt(n: Int) = (take(n), drop(n))

  private[this] def countWhile(p: A => Boolean): Int = {
    var result = 0
    val it = iterator
    while (it.hasNext && p(it.next())) result += 1
    result
  }
  override def dropWhile(p: A => Boolean) = drop(countWhile(p))
  override def takeWhile(p: A => Boolean) = take(countWhile(p))
  override def span(p: A => Boolean) = splitAt(countWhile(p))

  def this()(implicit ordering: Ordering[A]) = this(null)(ordering)

  private def newSetOrSelf(t: RB.Tree[A, Any]) = {
    if (t eq this.tree) this
    else new TreeSet[A](t)
  }

  /** A factory to create empty sets of the same type of keys.
   */
  override def empty = newSetOrSelf(null)

  /** Creates a new `TreeSet` with the entry added.
   *
   *  @param elem    a new element to add.
   *  @return        a new $coll containing `elem` and all the elements of this $coll.
   */
  def + (elem: A): TreeSet[A] = newSetOrSelf(RB.update(tree, elem, (), overwrite = false))

  /** A new `TreeSet` with the entry added is returned,
   *  assuming that elem is <em>not</em> in the TreeSet.
   *
   *  @param elem    a new element to add.
   *  @return        a new $coll containing `elem` and all the elements of this $coll.
   */
  def insert(elem: A): TreeSet[A] = {
    assert(!RB.contains(tree, elem))
    newSetOrSelf(RB.update(tree, elem, (), overwrite = false))
  }

  /** Creates a new `TreeSet` with the entry removed.
   *
   *  @param elem    a new element to add.
   *  @return        a new $coll containing all the elements of this $coll except `elem`.
   */
  def - (elem:A): TreeSet[A] =
    if (!RB.contains(tree, elem)) this
    else newSetOrSelf(RB.delete(tree, elem))

  /** Checks if this set contains element `elem`.
   *
   *  @param  elem    the element to check for membership.
   *  @return true, iff `elem` is contained in this set.
   */
  def contains(elem: A): Boolean = RB.contains(tree, elem)

  /** Creates a new iterator over all elements contained in this
   *  object.
   *
   *  @return the new iterator
   */
  def iterator: Iterator[A] = RB.keysIterator(tree)
  override def keysIteratorFrom(start: A): Iterator[A] = RB.keysIterator(tree, Some(start))

  override def foreach[U](f: A => U) = RB.foreachKey(tree, f)

  override def rangeImpl(from: Option[A], until: Option[A]): TreeSet[A] = newSetOrSelf(RB.rangeImpl(tree, from, until))
  override def range(from: A, until: A): TreeSet[A] = newSetOrSelf(RB.range(tree, from, until))
  override def from(from: A): TreeSet[A] = newSetOrSelf(RB.from(tree, from))
  override def to(to: A): TreeSet[A] = newSetOrSelf(RB.to(tree, to))
  override def until(until: A): TreeSet[A] = newSetOrSelf(RB.until(tree, until))

  override def firstKey = head
  override def lastKey = last


  private [collection] def addAll(ts: TreeSet[A]): TreeSet[A] = {
    assert (ordering == ts.ordering)
    newSetOrSelf(RB.union(tree, ts.tree))
  }
  private def sameCBF(bf: CanBuildFrom[_,_,_]): Boolean = {
    bf match {
      case tsb: TreeSet.TreeSetBuilder[_] if tsb.ordering == ordering => true
      case _ => false
    }
  }

  override def ++[B >: A, That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[TreeSet[A], B, That]): That = {
    that match {
      case ts: TreeSet[A] if ordering == ts.ordering  && sameCBF(bf) =>
        newSetOrSelf(RB.union(tree, ts.tree)).asInstanceOf[That]
      case _ =>
        super.++(that)
    }
  }

  override def ++:[B >: A, That](that: TraversableOnce[B])(implicit bf: CanBuildFrom[TreeSet[A], B, That]): That = {
    that match {
      case ts: TreeSet[A] if ordering == ts.ordering && sameCBF(bf) =>
        newSetOrSelf(RB.union(ts.tree, tree)).asInstanceOf[That]
      case _ =>
        super.++:(that)
    }
  }

  override def ++:[B >: A, That](that: scala.Traversable[B])(implicit bf: CanBuildFrom[TreeSet[A], B, That]): That = {
      that match {
        case ts: TreeSet[A] if ordering == ts.ordering && sameCBF(bf) =>
          newSetOrSelf(RB.union(ts.tree, tree)).asInstanceOf[That]
        case _ =>
          super.++:(that)
      }
  }

  override def --(xs: GenTraversableOnce[A]): TreeSet[A] = xs match {
    case ts: TreeSet[A] if ordering == ts.ordering =>
      newSetOrSelf(RB.difference(tree, ts.tree))
    case _ =>
      super.--(xs)
  }

  override def intersect(that: GenSet[A]): TreeSet[A] = that match {
    case ts: TreeSet[A] if ordering == ts.ordering =>
      newSetOrSelf(RB.intersect(tree, ts.tree))
    case _ =>
      super.intersect(that)
  }

  override def diff(that: GenSet[A]): TreeSet[A] = that match {
    case ts: TreeSet[A] if ordering == ts.ordering =>
      newSetOrSelf(RB.difference(tree, ts.tree))
    case _ =>
      super.diff(that)
  }

  override private[scala] def filterImpl(f: A => Boolean, isFlipped: Boolean) =
    newSetOrSelf(RB.filterKeys(tree, f, isFlipped))

  override def partition(p: A => Boolean): (TreeSet[A], TreeSet[A]) = {
    val (l, r) = RB.partitionKeys(tree, p)
    (newSetOrSelf(l), newSetOrSelf(r))
  }
}
