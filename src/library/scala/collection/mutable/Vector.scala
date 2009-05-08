/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
package scala.collection.mutable

import generic._

/** A subtrait of collection.Vector which represents sequences
 *  that can be mutated.
 */
trait Vector[A] extends Sequence[A] with collection.Vector[A] with MutableVectorTemplate[A, Vector[A]] {
  override protected[this] def newBuilder = Vector.newBuilder
  override def traversibleBuilder[B]: Builder[B, Vector[B], Any] = Vector.newBuilder[B]
}

object Vector extends SequenceFactory[Vector] {
  type Coll = Vector[_]
  implicit def builderFactory[A]: BuilderFactory[A, Vector[A], Coll] = new BuilderFactory[A, Vector[A], Coll] { def apply(from: Coll) = from.traversibleBuilder[A] }
  def newBuilder[A]: Builder[A, Vector[A], Any] = new ArrayBuffer
}
