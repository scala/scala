/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.collection
package immutable

import generic._
import mutable.Builder

/** A subtrait of `collection.SortedSet` which represents sorted sets
 *  which cannot be mutated.
 *
 *  @author Sean McDirmid
 *  @author Martin Odersky
 *  @version 2.8
 *  @since   2.4
 *  @define Coll immutable.SortedSet
 *  @define coll immutable sorted set
 */
trait SortedSet[A] extends Set[A] with scala.collection.SortedSet[A] with SortedSetLike[A, SortedSet[A]] {
  /** Needs to be overridden in subclasses. */
  override def empty: SortedSet[A] = SortedSet.empty[A]
}

/** $factoryInfo
 *  @define Coll immutable.SortedSet
 *  @define coll immutable sorted set
 */
object SortedSet extends ImmutableSortedSetFactory[SortedSet] {
  /** $sortedSetCanBuildFromInfo */
  implicit def canBuildFrom[A](implicit ord: Ordering[A]): CanBuildFrom[Coll, A, SortedSet[A]] = new SortedSetCanBuildFrom[A]
  def empty[A](implicit ord: Ordering[A]): SortedSet[A] = TreeSet.empty[A]
}
