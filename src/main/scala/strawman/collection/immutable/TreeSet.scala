package strawman.collection.immutable

import strawman.collection.mutable.Builder
import strawman.collection.{ConstrainedIterableFactory, Iterator}

import scala.{Boolean, Ordering}
import scala.Predef.???

/** Immutable sorted set backed by a tree */
final class TreeSet[A]()(implicit val ordering: Ordering[A])
  extends SortedSet[A]
    with SortedSetLike[A, TreeSet] {

  type CC[X] = TreeSet[X]

  // From IterableOnce
  def iterator(): Iterator[A] = ???

  // From FromIterable
  def fromIterable[B](coll: strawman.collection.Iterable[B]): Set[B] = ???

  // From IterableMonoTransforms
  protected[this] def fromIterableWithSameElemType(coll: strawman.collection.Iterable[A]): TreeSet[A] =
    TreeSet.constrainedNewBuilder[A].++=(coll).result

  // From ConstrainedFromIterable
  def constrainedFromIterable[B : Ordering](coll: strawman.collection.Iterable[B]): TreeSet[B] =
    TreeSet.constrainedNewBuilder[B].++=(coll).result
  def unconstrained: Set[A] = this

  // From SetLike
  def contains(elem: A): Boolean = ???
  def subsetOf(that: strawman.collection.Set[A]): Boolean = ???

  // From SetMonoTransforms
  def & (that: strawman.collection.Set[A]): TreeSet[A] = ???
  def ++ (that: strawman.collection.Set[A]): TreeSet[A] = ???

  // From immutable.SetMonoTransforms
  def +(elem: A): TreeSet[A] = ???
  def -(elem: A): TreeSet[A] = ???

  // From SortedLike
  def range(from: A, until: A): TreeSet[A] = ???
}

object TreeSet extends ConstrainedIterableFactory[TreeSet, Ordering] {

  def constrainedFromIterable[E : Ordering](it: strawman.collection.Iterable[E]): TreeSet[E] = ???

  def constrainedNewBuilder[A : Ordering]: Builder[A, TreeSet[A]] = ???
}
