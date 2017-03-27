package strawman.collection.immutable

import strawman.collection.mutable.Builder
import strawman.collection.{ConstrainedIterableFactory, ConstrainedPolyBuildable, Iterator}

import scala.{Boolean, Ordering}
import scala.Predef.???

/** Immutable sorted set backed by a tree */
final class TreeSet[A]()(implicit val ordering: Ordering[A])
  extends SortedSet[A]
    with SortedSetLike[A, TreeSet]
    with ConstrainedPolyBuildable[A, TreeSet, Ordering] {

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

  // From ConstrainedPolyBuildable
  def newConstrainedBuilder[E : Ordering] = TreeSet.constrainedNewBuilder

  // From PolyBuildable
  def newBuilder[E]: Builder[E, Set[E]] = ???

  // From SetLike
  def contains(elem: A): Boolean = ???

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
