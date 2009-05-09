/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
package scala.collection.immutable

import generic._
import mutable.ArrayBuffer

/** A subtrait of collection.Vector which represents sequences
 *  that cannot be mutated.
 */
trait Vector[+A] extends Sequence[A] with collection.Vector[A] with VectorTemplate[A, Vector[A]] { self =>
  override protected[this] def newBuilder = Vector.newBuilder
  override def traversableBuilder[B]: Builder[B, Vector[B]] = Vector.newBuilder[B]
}

object Vector extends SequenceFactory[Vector] {
  type Coll = Vector[_]
  class Impl[A](buf: ArrayBuffer[A]) extends Vector[A] { // todo: insert better vector implementation here
    def length = buf.length
    def apply(idx: Int) = buf.apply(idx)
  }
  implicit def builderFactory[A]: BuilderFactory[A, Vector[A], Coll] = new BuilderFactory[A, Vector[A], Coll] { def apply(from: Coll) = from.traversableBuilder[A] }
  def newBuilder[A]: Builder[A, Vector[A]] = new ArrayBuffer[A] mapResult (buf => new Impl(buf))
}
