package scala
package collection
package mutable

import generic._

/**
 * A mutable map whose keys are sorted.
 *
 * @tparam A the type of the keys contained in this sorted map.
 * @tparam B the type of the values associated with the keys.
 *
 * @author Rui Gonçalves
 * @version 2.12
 * @since 2.12
 *
 * @define Coll mutable.SortedMap
 * @define coll mutable sorted map
 */
trait SortedMap[A, B]
  extends Map[A, B]
  with collection.SortedMap[A, B]
  with MapLike[A, B, SortedMap[A, B]]
  with SortedMapLike[A, B, SortedMap[A, B]] {

  override protected[this] def newBuilder: Builder[(A, B), SortedMap[A, B]] = SortedMap.newBuilder[A, B]

  override def empty: SortedMap[A, B] = SortedMap.empty

  override def updated[B1 >: B](key: A, value: B1): SortedMap[A, B1] = this + ((key, value))

  override def +[B1 >: B](kv: (A, B1)): SortedMap[A, B1] = clone().asInstanceOf[SortedMap[A, B1]] += kv

  override def +[B1 >: B](elem1: (A, B1), elem2: (A, B1), elems: (A, B1)*): SortedMap[A, B1] =
    clone().asInstanceOf[SortedMap[A, B1]] += elem1 += elem2 ++= elems

  override def ++[B1 >: B](xs: GenTraversableOnce[(A, B1)]): SortedMap[A, B1] =
    clone().asInstanceOf[SortedMap[A, B1]] ++= xs.seq
}

/**
 * $factoryInfo
 *
 * @define Coll mutable.SortedMap
 * @define coll mutable sorted map
 */
object SortedMap extends MutableSortedMapFactory[SortedMap] {

  def empty[A, B](implicit ord: Ordering[A]): SortedMap[A, B] = TreeMap.empty[A, B]

  /** $sortedMapCanBuildFromInfo */
  implicit def canBuildFrom[A, B](implicit ord: Ordering[A]): CanBuildFrom[Coll, (A, B), SortedMap[A, B]] =
    new SortedMapCanBuildFrom[A, B]
}

/** Explicit instantiation of the `SortedMap` trait to reduce class file size in subclasses. */
abstract class AbstractSortedMap[A, B] extends scala.collection.mutable.AbstractMap[A, B] with SortedMap[A, B]
