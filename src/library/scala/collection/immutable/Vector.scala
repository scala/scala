/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package immutable

import generic._
import mutable.{Builder, ArrayBuffer}

/** A subtrait of <code>collection.Vector</code> which represents sequences
 *  that cannot be mutated.
 */
trait Vector[+A] extends Sequence[A]
                    with scala.collection.Vector[A]
                    with GenericTraversableTemplate[A, Vector]
                    with VectorLike[A, Vector[A]] {
  override def companion: GenericCompanion[Vector] = Vector
}

object Vector extends SequenceFactory[Vector] {
  // todo: insert better vector implementation here
  @serializable @SerialVersionUID(7129304555082767876L)
  class Impl[A](buf: ArrayBuffer[A]) extends Vector[A] {
    def length = buf.length
    def apply(idx: Int) = buf.apply(idx)
  }
  implicit def builderFactory[A]: BuilderFactory[A, Vector[A], Coll] =
    new VirtualBuilderFactory[A]
  def newBuilder[A]: Builder[A, Vector[A]] =
    new ArrayBuffer[A] mapResult (buf => new Impl(buf))
}
