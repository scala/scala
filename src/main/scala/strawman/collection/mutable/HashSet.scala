package strawman.collection.mutable

import strawman.collection.{IterableFactory, Iterator}

import scala.{Any, Boolean, Option, Unit}
import scala.Predef.???

/** Mutable set backed by a hash trie */
final class HashSet[A]
  extends Set[A]
    with SetLike[A, HashSet]
    with Buildable[A, HashSet[A]]
    with Builder[A, HashSet[A]] {

  def iterator(): Iterator[A] = ???

  def fromIterable[B](coll: strawman.collection.Iterable[B]): HashSet[B] =
    HashSet.fromIterable(coll)
  protected[this] def fromIterableWithSameElemType(coll: strawman.collection.Iterable[A]): HashSet[A] = fromIterable(coll)
  protected[this] def newBuilder: Builder[A, HashSet[A]] = new HashSet[A]
  def result: HashSet[A] = this

  def +=(elem: A): this.type = ???
  def -=(elem: A): this.type = ???
  def clear(): Unit = ???

  def contains(elem: A): Boolean = ???
  def get(elem: A): Option[A] = ???
  def subsetOf(that: strawman.collection.Set[A]): Boolean = ???

  def & (that: strawman.collection.Set[A]): HashSet[A] = ???
  def ++ (that: strawman.collection.Set[A]): HashSet[A] = ???

}

object HashSet extends IterableFactory[Any, HashSet] {

  def fromIterable[B](it: strawman.collection.Iterable[B]): HashSet[B] = {
    val result = new HashSet[B]
    for (elem <- it) {
      result += elem
    }
    result
  }

  def newBuilder[A]: Builder[A, HashSet[A]] = new HashSet[A]

}