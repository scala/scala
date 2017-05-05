package strawman
package collection
package mutable

import scala.{Any, Boolean, Option, Unit}
import scala.Predef.???

/** Mutable set backed by a hash trie */
final class HashSet[A]
  extends Set[A]
     with SetOps[A, HashSet, HashSet[A]]
     with Buildable[A, HashSet[A]]
     with Builder[A, HashSet[A]] {

  def iterator(): Iterator[A] = ???

  protected[this] def fromIterable[B](coll: strawman.collection.Iterable[B]): HashSet[B] =
    HashSet.fromIterable(coll)

  protected[this] def fromSpecificIterable(coll: collection.Iterable[A]): HashSet[A] = fromIterable(coll)

  protected[this] def newBuilder: Builder[A, HashSet[A]] = new HashSet[A]

  def result: HashSet[A] = this

  def add(elem: A): this.type = ???
  def subtract(elem: A): this.type = ???
  def clear(): Unit = ???

  def union(that: collection.Set[A]): HashSet[A] = ???
  def contains(elem: A): Boolean = ???
  def empty: HashSet[A] = HashSet.empty
  def get(elem: A): Option[A] = ???

  def concat(that: strawman.collection.Set[A]): HashSet[A] = ???

}

object HashSet extends IterableFactory[HashSet] {

  def fromIterable[B](it: strawman.collection.Iterable[B]): HashSet[B] = {
    val result = new HashSet[B]
    for (elem <- it) {
      result += elem
    }
    result
  }

  def newBuilder[A]: Builder[A, HashSet[A]] = new HashSet[A]

  def empty[A <: Any]: HashSet[A] = new HashSet[A]

}
