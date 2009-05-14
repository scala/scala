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
trait Vector[A] extends Sequence[A]
                   with collection.Vector[A]
                   with TraversableClass[A, Vector]
                   with MutableVectorTemplate[A, Vector[A]] {
  override def companion: Companion[Vector]  = Vector
}

object Vector extends SequenceFactory[Vector] {
  implicit def builderFactory[A]: BuilderFactory[A, Vector[A], Coll] = new VirtualBuilderFactory[A]
  def newBuilder[A]: Builder[A, Vector[A]] = new ArrayBuffer
}
