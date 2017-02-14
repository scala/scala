package strawman.collection.immutable

import strawman.collection.mutable.Builder
import strawman.collection.{Iterator, OrderingGuidedFactories}

import scala.{Boolean, Ordering}
import scala.Predef.???

/** Immutable sorted set backed by a tree */
final class TreeSet[A]()(implicit val ordering: Ordering[A])
  extends SortedSet[A]
    with SortedSetLike[A, TreeSet]{

  // From IterableOnce
  def iterator(): Iterator[A] = ???

  // From IterablePolyTransforms
  def fromIterable[B](coll: strawman.collection.Iterable[B]): Set[B] = ???
  protected[this] def fromIterableWithSameElemType(coll: strawman.collection.Iterable[A]): TreeSet[A] = TreeSet.builder[A].++=(coll).result

  // From SetLike
  def contains(elem: A): Boolean = ???
  def subsetOf(that: strawman.collection.Set[A]): Boolean = ???

  // From SetMonoTransforms
  def & (that: strawman.collection.Set[A]): TreeSet[A] = ???
  def ++ (that: strawman.collection.Set[A]): TreeSet[A] = ???

  // From SortedLike
  def range(from: A, until: A): TreeSet[A] = ???

  // From SortedPolyTransforms
  def map[B](f: (A) => B)(implicit ordering: Ordering[B]): TreeSet[B] = ???

  // From immutable.SetLike
  def +(elem: A): TreeSet[A] = ???
  def -(elem: A): TreeSet[A] = ???

}

object TreeSet extends OrderingGuidedFactories[TreeSet] {

  def builder[A](implicit ordering: Ordering[A]): Builder[A, TreeSet[A]] = ???

}