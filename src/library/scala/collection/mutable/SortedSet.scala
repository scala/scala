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
package mutable

import generic._

/**
 * Base trait for mutable sorted set.
 *
 * @define Coll `mutable.SortedSet`
 * @define coll mutable sorted set
 *
 * @author Lucien Pereira
 *
 */
trait SortedSet[A] extends scala.collection.SortedSet[A] with scala.collection.SortedSetLike[A,SortedSet[A]]
  with mutable.Set[A] with mutable.SetLike[A, SortedSet[A]] {

  /** Needs to be overridden in subclasses. */
  override def empty: SortedSet[A] = SortedSet.empty[A]

}

/**
 * A template for mutable sorted set companion objects.
 *
 * @define Coll `mutable.SortedSet`
 * @define coll mutable sorted set
 * @define factoryInfo
 *   This object provides a set of operations needed to create sorted sets of type mutable.SortedSet.
 * @define sortedSetCanBuildFromInfo
 *   Standard `CanBuildFrom` instance for sorted sets.
 *
 * @author Lucien Pereira
 *
 */
object SortedSet extends MutableSortedSetFactory[SortedSet] {
  def canBuildFrom[A](implicit ord: Ordering[A]): CanBuildFrom[Coll, A, SortedSet[A]] = new SortedSetCanBuildFrom[A]

  def empty[A](implicit ord: Ordering[A]): SortedSet[A] = TreeSet.empty[A]

  // Force a declaration here so that BitSet (which does not inherit from SortedSetFactory) can be more specific
  override implicit def newCanBuildFrom[A](implicit ord : Ordering[A]): CanBuildFrom[Coll, A, SortedSet[A]] = super.newCanBuildFrom
}

/** Explicit instantiation of the `SortedSet` trait to reduce class file size in subclasses. */
abstract class AbstractSortedSet[A] extends scala.collection.mutable.AbstractSet[A] with SortedSet[A]
