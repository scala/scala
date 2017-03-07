package strawman

package collection.immutable

import strawman.collection.mutable.Builder
import strawman.collection.{IterableFactory, Iterator}

import scala.{Any, Boolean}
import scala.Predef.???

/** An immutable Set backed by a hash trie */
class HashSet[A] extends Set[A] with SetLike[A, HashSet] {

  // From IterableOnce
  def iterator(): Iterator[A] = ???

  // From IterablePolyTransforms
  def fromIterable[B](coll: collection.Iterable[B]): HashSet[B] = HashSet.fromIterable(coll)
  protected[this] def fromIterableWithSameElemType(coll: collection.Iterable[A]): HashSet[A] = fromIterable(coll)

  // From SetLike
  def contains(elem: A): Boolean = ???
  def subsetOf(that: collection.Set[A]): Boolean = ???

  // From SetMonoTransforms
  def & (that: collection.Set[A]): HashSet[A] = ???
  def ++ (that: collection.Set[A]): HashSet[A] = ???

  // From immutable.SetLike
  def + (elem: A): HashSet[A] = ???
  def - (elem: A): HashSet[A] = ???

}

object HashSet extends IterableFactory[Any, HashSet] {

  def fromIterable[A](it: collection.Iterable[A]): HashSet[A] = ???

  def newBuilder[A]: Builder[A, HashSet[A]] = ???

}
