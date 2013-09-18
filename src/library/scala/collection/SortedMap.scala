/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection

import generic._
import mutable.Builder

/** A map whose keys are sorted.
 *
 *  @author Sean McDirmid
 *  @author Martin Odersky
 *  @version 2.8
 *  @since   2.4
 */
trait SortedMap[A, +B] extends Map[A, B] with SortedMapLike[A, B, SortedMap[A, B]] {
  /** Needs to be overridden in subclasses. */
  override def empty: SortedMap[A, B] = SortedMap.empty[A, B]

  override protected[this] def newBuilder: Builder[(A, B), SortedMap[A, B]] =
    immutable.SortedMap.newBuilder[A, B]
}

/**
 * @since 2.8
 */
object SortedMap extends SortedMapFactory[SortedMap] {
  def empty[A, B](implicit ord: Ordering[A]): SortedMap[A, B] = immutable.SortedMap.empty[A, B](ord)

  implicit def canBuildFrom[A, B](implicit ord: Ordering[A]): CanBuildFrom[Coll, (A, B), SortedMap[A, B]] = new SortedMapCanBuildFrom[A, B]

  private[collection] trait Default[A, +B] extends SortedMap[A, B] {
  self =>
    override def +[B1 >: B](kv: (A, B1)): SortedMap[A, B1] = {
      val b = SortedMap.newBuilder[A, B1]
      b ++= this
      b += ((kv._1, kv._2))
      b.result()
    }

    override def - (key: A): SortedMap[A, B] = {
      val b = newBuilder
      for (kv <- this; if kv._1 != key) b += kv
      b.result()
    }
  }
}
