/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.immutable

import generic._

/** A generic trait for immutable sets
 */
trait Set[A] extends Iterable[A] with collection.Set[A] with SetTemplate[A, Set[A]] { self =>

  override def empty = Set.empty

  override def hashCode = (Set.hashSeed /: this)(_ * 41 + _.hashCode)

  override def traversableBuilder[B]: Builder[B, Set[B], Any] = Set.newBuilder[B]
}

object Set extends SetFactory[Set] {
  private val hashSeed = "Set".hashCode
  type Coll = Set[_]
  implicit def builderFactory[A]: BuilderFactory[A, Set[A], Coll] = new BuilderFactory[A, Set[A], Coll] { def apply(from: Coll) = from.traversableBuilder[A] }
  def empty[A]: Set[A] = FlexSet.empty
}

