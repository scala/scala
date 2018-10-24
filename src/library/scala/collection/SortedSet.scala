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
import generic._

/** A sorted set.
 *
 *  @author Sean McDirmid
 *  @author Martin Odersky
 *  @since   2.4
 */
trait SortedSet[A] extends Set[A] with SortedSetLike[A, SortedSet[A]] {
  /** Needs to be overridden in subclasses. */
  override def empty: SortedSet[A] = SortedSet.empty[A]
}

/**
 * @since 2.8
 */
object SortedSet extends SortedSetFactory[SortedSet] {
  def empty[A](implicit ord: Ordering[A]): immutable.SortedSet[A] = immutable.SortedSet.empty[A](ord)
  def canBuildFrom[A](implicit ord: Ordering[A]): CanBuildFrom[Coll, A, SortedSet[A]] = newCanBuildFrom[A]
  // Force a declaration here so that BitSet (which does not inherit from SortedSetFactory) can be more specific
  override implicit def newCanBuildFrom[A](implicit ord : Ordering[A]) : CanBuildFrom[Coll, A, SortedSet[A]] = super.newCanBuildFrom
}
